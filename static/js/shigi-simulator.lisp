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
  (let* ((pointer (make-ecs-entity))
         (r 5)
         (model-offset (make-point-2d :x (* -1 r) :y (* -1 r))))
    (add-entity-tag pointer "mouse")
    (add-ecs-component-list
     pointer
     (make-point-2d)
     (make-model-2d :model (make-wired-regular-polygon :n 60 :r r
                                                       :color (get-param :cursor :color))
                    :depth 1
                    :offset model-offset)
     (make-script-2d :func (lambda (entity)
                             (with-ecs-components (point-2d) entity
                               (setf point-2d.x (get-mouse-x))
                               (setf point-2d.y (get-mouse-y)))))
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
                          :color 0x00ff00 :z 1))
    (scene.add (make-line :pos-a (list (/ width 2) 0) :pos-b (list (/ width 2) height)
                          :color 0x00ff00 :z 1))))

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
      (add-rect 0 0 screen-width offset-y)
      (add-rect 0 0 offset-x screen-height)
      (let ((offset (+ offset-y area-height)))
        (add-rect 0 offset screen-width (- screen-height offset)))
      (let ((offset (+ offset-x area-width)))
        (add-rect offset 0 (- screen-width offset) screen-height)))))

(defun.ps init (scene)
  (add-axis-to-scene scene)
  (add-frame-to-scene scene)
  (make-sample-entities)
  (generate-color-grid))

(defun.ps main ()
  (start-game :camera-offset-x (get-param :play-area :x)
              :camera-offset-y (get-param :play-area :y)
              :screen-width screen-width
              :screen-height screen-height
              :init-function init))

(defun js-main ()
  (with-use-ps-pack (:cl-shigi-simulator.static.js.tools
                     :cl-shigi-simulator.static.js.shigi
                     :cl-shigi-simulator.static.js.player
                     :cl-shigi-simulator.static.js.color-chip
                     :cl-shigi-simulator.static.js.basic-ecs
                     :this)
    (initialize-input)
    (window.add-event-listener "DOMContentLoaded" main false))) 
