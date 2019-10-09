(in-package :cl-user)
(defpackage cl-shigi-simulator.static.js.shigi-simulator
  (:use :cl
        :cl-ppcre
        :parenscript
        :cl-ps-ecs
        :ps-experiment
        :cl-web-2d-game/core/basic-components
        :cl-web-2d-game/core/initializer
        :cl-web-2d-game/graphics/2d-geometry
        :cl-web-2d-game/graphics/draw-model-system
        :cl-web-2d-game/inputs/input
        :cl-web-2d-game/physics/collision
        :cl-web-2d-game/physics/collision-system
        :cl-web-2d-game/utils/debug/logger)
  (:import-from :ps-experiment/common-macros
                :setf-with)
  (:import-from :ps-experiment
                :defun.ps
                :defvar.ps
                :with-use-ps-pack)
  (:import-from :cl-ps-ecs
                :with-ecs-components
                :do-ecs-entities)
  (:import-from :cl-shigi-simulator.static.js.color-chip
                :generate-color-grid)
  (:import-from :cl-shigi-simulator.static.js.player
                :make-player)
  (:import-from :cl-shigi-simulator.static.js.shigi
                :make-shigi)
  (:import-from :cl-shigi-simulator.static.js.tools
                :get-param
                :start-game
                :shigi-screen-width
                :shigi-screen-height))
(in-package :cl-shigi-simulator.static.js.shigi-simulator)

(defun.ps+ make-mouse-pointer ()
  (let* ((pointer (make-ecs-entity))
         (r 5))
    (add-entity-tag pointer "mouse")
    (add-ecs-component-list
     pointer
     (make-point-2d)
     (make-model-2d :model (make-wired-circle :r r
                                              :color (get-param :cursor :color))
                    :depth 1)
     (make-script-2d :func (lambda (entity)
                             (with-ecs-components (point-2d) entity
                               (setf (point-2d-x point-2d) (get-mouse-x))
                               (setf (point-2d-y point-2d) (get-mouse-y)))))
     (make-physic-circle :r r
                         :target-tags '("shigi-part")))
    (add-ecs-entity pointer)))

(defun.ps make-sample-entities ()
  (make-player)
  (make-shigi)
  (make-mouse-pointer))

(defun.ps add-axis-to-scene (scene)
  (let ((width (get-param :play-area :width))
        (height (get-param :play-area :height)))
    (scene.add (make-line :pos-a (list width (/ height 2)) :pos-b (list 0 (/ height 2))
                          :color 0x00ff00))
    (scene.add (make-line :pos-a (list (/ width 2) 0) :pos-b (list (/ width 2) height)
                          :color 0x00ff00))))

(defun.ps add-frame-to-scene (scene)
  (let ((area-width (get-param :play-area :width))
        (area-height (get-param :play-area :height))
        (offset-x (get-param :play-area :x))
        (offset-y (get-param :play-area :y)))
    (labels ((add-rect (x y width height)
               (let ((frame (make-ecs-entity)))
                 (add-ecs-component-list
                  frame
                  (make-model-2d :model (make-solid-rect :width width :height height
                                                         :color 0x000000)
                                 :depth 999)
                  (make-point-2d :x (- x offset-x) :y (- y offset-y)))
                 (add-ecs-entity frame))))
      (add-rect 0 0 shigi-screen-width offset-y)
      (add-rect 0 0 offset-x shigi-screen-height)
      (let ((offset (+ offset-y area-height)))
        (add-rect 0 offset shigi-screen-width (- shigi-screen-height offset)))
      (let ((offset (+ offset-x area-width)))
        (add-rect offset 0 (- shigi-screen-width offset) shigi-screen-height)))))

(defun.ps+ init (scene)
  (setf-collider-model-enable nil)
  (add-axis-to-scene scene)
  (add-frame-to-scene scene)
  (make-sample-entities)
  (generate-color-grid))

(defun.ps update ()
  (add-to-monitoring-log (+ "Entity count: "
                            (let ((sum 0))
                              (do-ecs-entities entity
                                (incf sum))
                              sum))))

(defun.ps+ main ()
  (start-game :camera-offset-x (get-param :play-area :x)
              :camera-offset-y (get-param :play-area :y)
              :screen-width shigi-screen-width
              :screen-height shigi-screen-height
              :init-function #'init
              :update-function #'update))

(defvar.ps+ *max-event-log-count* 10)

(defun js-main ()
  (with-use-ps-pack (:this)
    (init-input)
    (window.add-event-listener "DOMContentLoaded" main false))) 
