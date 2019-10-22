(defpackage cl-shigi-simulator/static/js/test/multiple
  (:use :cl
        :cl-ppcre
        :parenscript
        :ps-experiment
        :cl-web-2d-game
        :cl-shigi-simulator/static/js/tools
        :cl-ps-ecs)
  (:import-from :cl-shigi-simulator/static/js/player
                :make-player)
  (:import-from :ps-experiment/common-macros
                :with-slots-pair))
(in-package :cl-shigi-simulator/static/js/test/multiple)

(defun.ps+ init-mouse-pointer ()
  (let ((pointer (make-ecs-entity))
        (r #y8))
    (add-ecs-component-list
     pointer
     (make-point-2d)
     (make-model-2d :model (make-solid-circle :r r :color #xff0000)
                    :depth (get-depth :mouse))
     (make-script-2d :func (lambda (entity)
                             (with-ecs-components (point-2d) entity
                               (setf (point-2d-x point-2d) (get-mouse-x)
                                     (point-2d-y point-2d) (get-mouse-y))))))
    (add-ecs-entity pointer)))

(defun.ps+ init-global-parent ()
  (let ((bg (make-ecs-entity))
        (sw shigi-screen-width)
        (sh shigi-screen-height)
        (width  #lx1000)
        (height #ly1000)
        (db (get-depth :background))
        (df (get-depth :foregrond)))
    (flet ((make-model (x y w h depth color)
             (make-model-2d :model (make-solid-rect :width  w
                                                    :height h
                                                    :color color)
                            :depth depth
                            :offset (make-point-2d :x x :y y))))
      (add-ecs-component-list
       bg
       (make-point-2d :x (/ (- sw width)  2)
                      :y (/ (- sh height) 2))
       (make-model 0 0 width height db #xffffff)
       ;; wallpapers
       (make-model (* -1 sw) 0
                   sw sh df #x000000)
       (make-model width 0
                   sw sh df #x000000)
       (make-model 0 (* -1 sh)
                   sw sh df #x000000)
       (make-model 0 height
                   sw sh df #x000000)))
    (add-ecs-entity bg)))

(defun.ps+ init (scene)
  (declare (ignore scene))
  (setf-collider-model-enable nil)
  (init-mouse-pointer)
  (let ((parent (init-global-parent)))
    (stack-default-ecs-entity-parent parent)
    (make-player)))

(defun.ps+ main ()
  (start-game :screen-width shigi-screen-width
              :screen-height shigi-screen-height
              :init-function #'init))

(defun js-main ()
  (with-use-ps-pack (:cl-shigi-simulator/static/js/tools
                     :cl-shigi-simulator/static/js/basic-ecs
                     :this)
    (init-input)
    (window.add-event-listener "DOMContentLoaded" main false))) 
