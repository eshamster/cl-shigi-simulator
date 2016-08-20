(in-package :cl-user)
(defpackage cl-shigi-simulator.static.js.player
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :parenscript
        :cl-shigi-simulator.static.js.tools))
(in-package :cl-shigi-simulator.static.js.player)
(enable-ps-experiment-syntax)

(defun.ps make-player-ring ()
  (let ((ring (make-ecs-entity))
        (r (get-param :player :ring-r)))
    (add-ecs-component-list
     ring
     (make-model-2d :model (make-wired-regular-polygon :r r :n 100 :color 0x4444ff)
                 :depth (get-param :player :depth))
     (make-point-2d :x (* r -1) :y (* r -1)))
    ring))

(defun.ps make-player-body ()
  (let ((body (make-ecs-entity))
        (r (get-param :player :body-r)))
    (add-ecs-component-list
     body
     (make-model-2d :model (make-solid-regular-polygon :r r :n 100 :color 0x4444ff)
                 :depth (get-param :player :depth))
     (make-point-2d :x (* r -1) :y (* r -1)))
    body))

(defun.ps make-player-center ()
  (let ((body (make-ecs-entity))
        (speed (get-param :player :speed)))
    (add-ecs-component-list
     body
     (make-point-2d :x #y(* 500 4/3) :y #y100)
     (macrolet ((move (direction move)
                  `(when (is-key-down ,direction) ,move)))
       (make-script-2d :func (lambda (entity)
                               (with-ecs-components (point-2d) entity
                                 (move :left  (decf point-2d.x speed))
                                 (move :right (incf point-2d.x speed))
                                 (move :down  (decf point-2d.y speed))
                                 (move :up    (incf point-2d.y speed)))))))
    body))

(defun.ps make-player ()
  (let ((center (make-player-center))
        (body (make-player-body))
        (ring (make-player-ring)))
    (add-ecs-entity center)
    (add-ecs-entity body center)
    (add-ecs-entity ring center)))
