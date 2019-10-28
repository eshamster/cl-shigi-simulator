(defpackage cl-shigi-simulator/static/js/shigi-simulator
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
  (:import-from :cl-shigi-simulator/static/js/color-chip
                :generate-color-grid)
  (:import-from :cl-shigi-simulator/static/js/player
                :make-player)
  (:import-from :cl-shigi-simulator/static/js/player-observer
                :make-player-observer)
  (:import-from :cl-shigi-simulator/static/js/shigi
                :make-shigi)
  (:import-from :cl-shigi-simulator/static/js/tools
                :get-depth
                :get-param
                :start-game
                :shigi-screen-width
                :shigi-screen-height))
(in-package :cl-shigi-simulator/static/js/shigi-simulator)

(defun.ps+ make-mouse-pointer ()
  (let* ((pointer (make-ecs-entity))
         (r 5))
    (add-entity-tag pointer :mouse)
    (add-ecs-component-list
     pointer
     (make-point-2d)
     (make-model-2d :model (make-wired-circle :r r
                                              :color (get-param :cursor :color))
                    :depth (get-depth :mouse))
     (make-script-2d :func (lambda (entity)
                             (with-ecs-components (point-2d) entity
                               (setf (point-2d-x point-2d) (get-mouse-x))
                               (setf (point-2d-y point-2d) (get-mouse-y)))))
     (make-physic-circle :r r
                         :target-tags '(:shigi-part)))
    (add-ecs-entity pointer)))

(defun.ps+ make-sample-entities ()
  (let ((player (make-player)))
    (make-player-observer player))
  (make-shigi)
  (make-mouse-pointer))

(defun.ps+ add-axis-to-scene ()
  (let ((width (get-param :play-area :width))
        (height (get-param :play-area :height))
        (entity (make-ecs-entity)))
    (add-ecs-component-list
     entity
     (make-point-2d)
     (make-model-2d :model (make-line :pos-a (list width (/ height 2))
                                      :pos-b (list 0 (/ height 2))
                                      :color #x00ff00))
     (make-model-2d :model (make-line :pos-a (list (/ width 2) 0)
                                      :pos-b (list (/ width 2) height)
                                      :color #x00ff00)))
    (add-ecs-entity entity)))

(defun.ps+ add-frame-to-scene ()
  (let ((area-width (get-param :play-area :width))
        (area-height (get-param :play-area :height))
        (offset-x (get-param :play-area :x))
        (offset-y (get-param :play-area :y)))
    (labels ((add-rect (x y width height)
               (let ((frame (make-ecs-entity)))
                 (add-ecs-component-list
                  frame
                  (make-model-2d :model (make-solid-rect :width width :height height
                                                         :color #x000000)
                                 :depth (get-depth :foregrond))
                  (make-point-2d :x (- x offset-x) :y (- y offset-y)))
                 (add-ecs-entity frame))))
      (add-rect 0 0 shigi-screen-width offset-y)
      (add-rect 0 0 offset-x shigi-screen-height)
      (let ((offset (+ offset-y area-height)))
        (add-rect 0 offset shigi-screen-width (- shigi-screen-height offset)))
      (let ((offset (+ offset-x area-width)))
        (add-rect offset 0 (- shigi-screen-width offset) shigi-screen-height)))))

(defun.ps+ init (scene)
  (declare (ignore scene))
  (setf-collider-model-enable nil)
  (add-axis-to-scene)
  (add-frame-to-scene)
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

(defun js-main ()
  (with-use-ps-pack (:this)
    (init-input)
    (window.add-event-listener "DOMContentLoaded" main false))) 
