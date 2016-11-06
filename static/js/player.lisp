(in-package :cl-user)
(defpackage cl-shigi-simulator.static.js.player
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :parenscript
        :cl-web-2d-game
        :cl-shigi-simulator.static.js.shigi
        :cl-shigi-simulator.static.js.tools)
  (:import-from :ps-experiment.common-macros
                :with-slots-pair))
(in-package :cl-shigi-simulator.static.js.player)

(enable-ps-experiment-syntax)

;; --- lazer (kaihou) --- ;;

(defun.ps+ angle-to-target (lazer target)
  (let ((lazer-pnt (calc-global-point lazer))
        (target-pnt (calc-global-point target)))
    (vector-angle (decf-vector (clone-vector target-pnt) lazer-pnt))))

(defun.ps+ turn-lazer-to-target (lazer)
  (with-ecs-components (speed-2d) lazer
    (let* ((target (get-entity-param lazer :target))
           (now-angle (vector-angle speed-2d))
           (target-angle (angle-to-target lazer target)))
      (setf-vector-angle speed-2d
                         (adjust-to-target now-angle target-angle
                                           (get-param :lazer :max-rot-speed))))))

(defun.ps update-lazer-tails (lazer)
  ;; Note that a model space is relative to the point of the entity. (Ex. (0, 0) in the model space is the same to the point of the entity in the absolute coordinate.)
  (check-entity-tags "lazer")
  (with-ecs-components ((new-point point-2d) model-2d) lazer
    (let* ((geometry model-2d.model.geometry)
           (pnt-list geometry.vertices)
           (len (length pnt-list)))
      (with-slots-pair (((pre-x x) (pre-y y)) (get-entity-param lazer :pre-point)
                        ((new-x x) (new-y y)) new-point)
        (dotimes (i (1- len))
          (let ((index (- len (1+ i))))
            (with-slots-pair (((x1 x) (y1 y)) (aref pnt-list index)
                              ((x0 x) (y0 y)) (aref pnt-list (1- index)))
              (setf x1 (- x0 (- new-x pre-x))
                    y1 (- y0 (- new-y pre-y))))))
        (setf pre-x new-x
              pre-y new-y))
      (setf geometry.vertices-need-update t))))

;; only prototype to delete an entity.
(defun.ps sample-to-delete (entity)
  (check-entity-tags "lazer")
  (cond ((not (get-entity-param entity :hitp))
         (with-ecs-components (speed-2d) entity
           (let ((max-duration (get-entity-param entity :max-duration)))
             (when (= max-duration 0)
               (set-entity-param entity :hitp t))
             (set-entity-param entity :max-duration (1- max-duration)))))
        (t (let ((duration (get-entity-param entity :duration)))
             (when (< duration 0)
               (delete-ecs-entity entity))
             (set-entity-param entity :duration (1- duration))))))

(defun.ps process-lazer-collision (mine target)
  (when (= (ecs-entity-id target)
           (ecs-entity-id (get-entity-param mine :target)))
    (with-ecs-components (speed-2d) mine
      (setf speed-2d.x 0
            speed-2d.y 0)
      (set-entity-param mine :hitp t))))

(defun.ps make-lazer (player target)
  (check-entity-tags player "player")
  (let ((lazer (make-ecs-entity))
        (num-pnts (get-param :lazer :tail-length))
        (pnt-list '()))
    (add-entity-tag lazer "lazer")
    (with-ecs-components (point-2d) player
      (with-slots (x y) point-2d
        (dotimes (i num-pnts)
          (push (list 0 0) pnt-list))
        (add-ecs-component-list
         lazer
         (make-point-2d :x x :y y)
         (make-model-2d :model (make-lines :pnt-list pnt-list :color 0xff0000)
                        :depth (get-param :lazer :depth))
         (make-speed-2d :x (get-param :lazer :max-speed))
         (make-script-2d :func #'(lambda (entity)
                                   (turn-lazer-to-target entity)
                                   (update-lazer-tails entity)
                                   (sample-to-delete entity)))
         (make-physic-circle :r 0 :on-collision #'process-lazer-collision)
         (init-entity-params :duration num-pnts
                             :hitp nil
                             :max-duration 600
                             :pre-point (make-vector-2d :x x :y y)
                             :target target))))
    lazer))

;; --- body --- ;;

(defun.ps make-player-ring ()
  (let ((ring (make-ecs-entity))
        (r (get-param :player :ring-r)))
    (add-ecs-component-list
     ring
     (make-model-2d :model (make-wired-regular-polygon :r r :n 100
                                                       :color (get-param :player :color))
                 :depth (get-param :player :depth))
     (make-point-2d :x (* r -1) :y (* r -1)))
    ring))

(defun.ps make-player-body ()
  (let ((body (make-ecs-entity))
        (r (get-param :player :body-r)))
    (add-ecs-component-list
     body
     (make-model-2d :model (make-solid-regular-polygon :r r :n 100
                                                       :color (get-param :player :color))
                 :depth (get-param :player :depth))
     (make-point-2d :x (* r -1) :y (* r -1)))
    body))

(defun.ps shot-lazer (player)
  (when (is-key-down-now :x)
    (let* ((pnt (calc-global-point player))
           (target (get-nearest-shigi-part pnt))
           (lazer (make-lazer player target)))
      (add-ecs-entity-to-buffer lazer))))

(defun.ps move-player (player)
  (let ((speed (get-param :player :speed))
        (r (get-param :player :body-r)))
    (with-ecs-components (point-2d) player
      (macrolet ((move (direction move)
                   `(when (is-key-down ,direction) ,move)))
        (move :left  (decf point-2d.x speed))
        (move :right (incf point-2d.x speed))
        (move :down  (decf point-2d.y speed))
        (move :up    (incf point-2d.y speed)))
      (macrolet ((fix-position (op place value)
                   `(when (,op ,place ,value)
                      (setf ,place ,value))))
        (fix-position < point-2d.x r)
        (fix-position > point-2d.x (- (get-param :play-area :width) r))
        (fix-position < point-2d.y r)
        (fix-position > point-2d.y (- (get-param :play-area :height) r))))))

(defun.ps make-player-center ()
  (let ((body (make-ecs-entity)))
    (add-entity-tag body "player")
    (add-ecs-component-list
     body
     (make-point-2d :x (/ (get-param :play-area :width) 2) :y #y100)
     (make-script-2d :func #'(lambda (player)
                               (move-player player)
                               (shot-lazer player))))
    body))

(defun.ps make-player ()
  (let ((center (make-player-center))
        (body (make-player-body))
        (ring (make-player-ring)))
    (add-ecs-entity center)
    (add-ecs-entity body center)
    (add-ecs-entity ring center)))
