(in-package :cl-user)
(defpackage cl-shigi-simulator.static.js.shigi-simulator
  (:use :cl
        :cl-ppcre
        :parenscript)
  (:import-from :ps-experiment.common-macros
                :setf-with)
  (:import-from :ps-experiment
                :defun.ps
                :defvar.ps
                :with-use-ps-pack) 
  (:import-from :cl-ps-ecs
                :with-ecs-components)
  (:import-from :cl-shigi-simulator.static.js.tools
                :get-param))
(in-package :cl-shigi-simulator.static.js.shigi-simulator)

(defun.ps make-mouse-pointer ()
  (let ((pointer (make-ecs-entity))
        (r 5))
    (add-entity-tag pointer "mouse")
    (add-ecs-component-list
     pointer
     (make-point-2d :center (make-vector-2d :x r :y r))
     (make-model-2d :model (make-wired-regular-polygon :n 60 :r r
                                                       :color (get-param :cursor :color))
                    :depth 1)
     (make-script-2d :func (lambda (entity)
                             (with-ecs-components (point-2d) entity
                               (setf point-2d.x (get-mouse-x))
                               (setf point-2d.y (get-mouse-y)))))
     (make-physic-circle :r r))
    (add-ecs-entity pointer)))

(defun.ps make-sample-entities ()
  (make-player)
  (make-shigi)
  (make-mouse-pointer))

(defun.ps init (scene)
  (scene.add (make-line :pos-a (list #y1333 #y500) :pos-b (list 0 #y500) :color 0x00ff00 :z 1))
  (scene.add (make-line :pos-a (list #y666 #y0) :pos-b (list #y666 #y1000) :color 0x00ff00 :z 1))
  (make-sample-entities)
  (generate-color-grid))

(defun.ps update ()
  (clear-debug-area)
  (process-input)
  (ecs-main))

(defun.ps main ()
  (start-game screen-width screen-height init update))

(defun js-main ()
  (with-use-ps-pack (:cl-shigi-simulator.static.js.2d-geometry
                     :cl-shigi-simulator.static.js.tools
                     :cl-shigi-simulator.static.js.input
                     :cl-shigi-simulator.static.js.shigi
                     :cl-shigi-simulator.static.js.player
                     :cl-shigi-simulator.static.js.color-chip
                     :cl-shigi-simulator.static.js.basic-ecs
                     :this)
    (window.add-event-listener "mousemove" on-mouse-move-event)
    (window.add-event-listener "mousedown" on-mouse-down-event)
    (window.add-event-listener "mouseup" on-mouse-up-event)
    (window.add-event-listener "touchstart" on-touch-start)
    (window.add-event-listener "touchend" on-touch-end)
    (window.add-event-listener "touchmove" on-touch-move-event)
    (window.add-event-listener "keydown" (lambda (e) (e.prevent-default)))
    (window.add-event-listener "DOMContentLoaded" main false))) 
