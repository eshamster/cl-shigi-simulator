(in-package :cl-user)
(defpackage cl-shigi-simulator.static.js.player
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :parenscript
        :cl-web-2d-game
        :cl-shigi-simulator.static.js.tools))
(in-package :cl-shigi-simulator.static.js.player)

(enable-ps-experiment-syntax)

;; --- lazer (kaihou) --- ;;

(defun.ps make-lazer+ (player)
  (check-entity-tags player "player")
  (let ((lazer (make-ecs-entity)))
    ;;--- TODO: (Now, only sample to test dynamical generating or deleting eneity.)
    (add-entity-tag lazer "lazer")
    (with-ecs-components (point-2d) player
      (add-ecs-component-list
       lazer
       (make-point-2d :x (point-2d-x point-2d)
                      :y (point-2d-y point-2d))
       (make-model-2d :model (make-solid-regular-polygon :r 10 :n 6
                                                         :color #xff0000)
                      :depth (get-param :lazer :depth))
       (make-script-2d :func #'(lambda (entity)
                                 (let ((duration (get-entity-param entity :duration)))
                                   (when (< duration 0)
                                     (delete-ecs-entity entity))
                                   (set-entity-param entity :duration (1- duration)))))
       (init-entity-params :duration 120)))
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
    (let ((lazer (make-lazer player)))
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
