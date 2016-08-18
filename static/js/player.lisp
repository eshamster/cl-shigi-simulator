(in-package :cl-user)
(defpackage cl-shigi-simulator.static.js.player
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :parenscript))
(in-package :cl-shigi-simulator.static.js.player)

(enable-ps-experiment-syntax)

(defun.ps make-player-model ()
  (make-model-2d :model (make-wired-regular-polygon :r #y70 :n 100 :color 0x4444ff)
                 :depth 0.5))

(defun.ps make-player ()
  (let ((player (make-ecs-entity)))
    (add-ecs-component-list
     player
     (make-player-model)
     (make-point-2d :x #y(* 500 4/3) :y #y100)
     (make-script-2d :func (lambda (entity)
                             (with-ecs-components (point-2d) entity
                               (when (is-key-down-now :b)
                                 (setf point-2d.x 0)
                                 (setf point-2d.y 0))))))
    (add-ecs-entity player)))
