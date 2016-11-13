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
  (unless (or (get-entity-param lazer :stop-homing-p)
              (null (get-entity-param lazer :target)))
    (with-ecs-components (speed-2d) lazer
      (let* ((target (get-entity-param lazer :target))
             (now-angle (vector-angle speed-2d))
             (target-angle (angle-to-target lazer target)))
        (setf-vector-angle speed-2d
                           (adjust-to-target now-angle target-angle
                                             (get-param :lazer :max-rot-speed)))))))

(defun.ps update-lazer-tails (lazer)
  ;; Note that a model space is relative to the point of the entity. (Ex. (0, 0) in the model space is the same to the point of the entity in the absolute coordinate.)
  (check-entity-tags "lazer")
  (with-ecs-components ((new-point point-2d) model-2d) lazer
    (let* ((geometry model-2d.model.geometry)
           (pnt-list geometry.vertices)
           (pre-point (get-entity-param lazer :pre-point))
           (len (length pnt-list)))
      (with-slots-pair (((pre-x x) (pre-y y)) pre-point
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
  (let ((true-target (get-entity-param mine :target)))
    (when (and true-target
               (not (get-entity-param mine :stop-homing-p))
               (= (ecs-entity-id target)
                  (ecs-entity-id true-target)))
      (with-ecs-components (speed-2d) mine
        (setf speed-2d.x 0
              speed-2d.y 0)
        (set-entity-param mine :hitp t)))))

;; The base angle of first-angle is the angle of (0, -1)
(defun.ps make-lazer (&key player target first-angle first-offset)
  (check-entity-tags player "player")
  (check-type first-offset vector-2d)
  (when target
    (check-entity-tags target "shigi-part"))
  (let ((lazer (make-ecs-entity))
        (num-pnts (get-param :lazer :tail-length))
        (first-speed (make-speed-2d :x (get-param :lazer :max-speed)))
        (pnt-list '()))
    (add-entity-tag lazer "lazer")
    (setf-vector-angle first-speed (- first-angle (/ PI 2)))
    (with-ecs-components (point-2d) player
      (with-slots (x y) point-2d
        (dotimes (i num-pnts)
          (push (list 0 0) pnt-list))
        (let ((first-x (+ x (vector-2d-x first-offset)))
              (first-y (+ y (vector-2d-y first-offset))))
          (add-ecs-component-list
           lazer
           (make-point-2d :x first-x :y first-y)
           (make-model-2d :model (make-lines :pnt-list pnt-list :color 0xff0000)
                          :depth (get-param :lazer :depth))
           first-speed
           (make-script-2d :func #'(lambda (entity)
                                     (let ((target (get-entity-param entity :target)))
                                       (unless (and target
                                                    (shigi-part-valid-p target))
                                         (set-entity-param entity :stop-homing-p t)))
                                     (turn-lazer-to-target entity)
                                     (update-lazer-tails entity)
                                     (sample-to-delete entity)))
           (make-physic-circle :r 0 :on-collision #'process-lazer-collision)
           (init-entity-params :duration num-pnts
                               :hitp nil
                               :stop-homing-p nil
                               :max-duration 600
                               :pre-point (make-vector-2d :x first-x :y first-y)
                               :target target)))))
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

(defvar.ps+ *player* nil)

(defun.ps+ trigger-player-lazer ()
  (when *player*
    (set-entity-param *player* :lazer-triggered-p t)))

(defun.ps shot-lazer (player)
  (check-entity-tags player "player")
  (when (get-entity-param *player* :lazer-triggered-p)
    (set-entity-param *player* :lazer-triggered-p nil)
    (let* ((pnt (calc-global-point player))
           (target (get-nearest-shigi-part pnt))
           (min-angle (get-param :all-lazer :min-angle))
           (max-angle (get-param :all-lazer :max-angle))
           (half-num (get-param :all-lazer :half-num))
           (offset-x (get-param :all-lazer :first-offset :x))
           (offset-y (get-param :all-lazer :first-offset :y)))
      (dotimes (i (get-param :all-lazer :half-num))
        (let ((angle (lerp-scalar min-angle max-angle
                                  (/ i (1- half-num)))))
          (dolist (rightp '(t nil))
            (add-ecs-entity-to-buffer
             (make-lazer :player player
                         :target target
                         :first-angle (* angle (if rightp 1 -1))
                         :first-offset (make-vector-2d :x (* offset-x (if rightp 1 -1))
                                                       :y offset-y)))))))))

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

(defun.ps control-player (player)
  (declare (ignore player))
  (when (is-key-down-now :x)
    (trigger-player-lazer)))

(defstruct.ps+ nearest-part-register (part-id -1) (frame-count -1))

(defun.ps display-nearest-part (part-id frame-count)
  (let ((part (find-a-entity #'(lambda (entity)
                                 (= (ecs-entity-id entity) part-id)))))
    (labels ((pad (str len)
               ;; easy impremetation
               ((@ (+ "      " str) slice) (* len -1))))
      (push-log-text (+ (pad (get-entity-param part :display-name) 6) ":"
                        (pad (floor (* frame-count 1000/60)) 4) "ms ("
                        (pad frame-count 3) "F)")))))

(defun.ps register-nearest-part (player)
  (check-entity-tags player "player")
  (let* ((register (get-entity-param player :nearest-part-register))
         (nearest (get-nearest-shigi-part (calc-global-point player)))
         (nearest-id (if nearest (ecs-entity-id nearest) -1)))
    (with-slots (part-id frame-count) register
      (if (= part-id nearest-id)
          (incf frame-count)
          (progn (when (>= part-id 0)
                   (display-nearest-part part-id frame-count))
                 (setf part-id nearest-id
                       frame-count 0))))))

(defun.ps make-player-center ()
  (let ((body (make-ecs-entity)))
    (add-entity-tag body "player")
    (add-ecs-component-list
     body
     (make-point-2d :x (/ (get-param :play-area :width) 2) :y #y100)
     (make-script-2d :func #'(lambda (player)
                               (control-player player)
                               (move-player player)
                               (register-nearest-part player)
                               (shot-lazer player)))
     (init-entity-params :lazer-triggered-p nil
                         :nearest-part-register (make-nearest-part-register)))
    body))

(defun.ps make-player ()
  (let ((center (make-player-center))
        (body (make-player-body))
        (ring (make-player-ring)))
    (add-ecs-entity center)
    (add-ecs-entity body center)
    (add-ecs-entity ring center)
    (setf *player* center)))
