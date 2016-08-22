(in-package :cl-user)
(defpackage cl-shigi-simulator.static.js.test.collision
  (:use :cl
        :cl-ppcre
        :parenscript
        :ps-experiment
        :cl-ps-ecs))
(in-package :cl-shigi-simulator.static.js.test.collision)

(defvar.ps stats nil)

(defun.ps init-stats ()
  (let ((stats (new (-stats))))
    (stats.set-mode 0)
    (with-slots (position left top) stats.dom-element.style
      (setf position "absolute")
      (setf left "0px")
      (setf top "0px"))
    ((@ (document.get-element-by-id "stats-output") append-child) stats.dom-element)
    stats))

(defun.ps init-camera (width height)
  (let* ((z 1000)
         (camera (new (#j.THREE.OrthographicCamera#
                       0 width height 0 0 (* z 2)))))
    (camera.position.set 0 0 z)
    camera))

(defun.ps make-mouse-pointer ()
  (let ((pointer (make-ecs-entity))
        (r 5))
    (add-ecs-component-list
     pointer
     (make-point-2d :center (make-vector-2d :x r :y r))
     (make-model-2d :model (make-wired-regular-polygon :n 60 :color 0xff0000 :r r)
                    :depth 1)
     (make-script-2d :func (lambda (entity)
                             (with-ecs-components (point-2d) entity
                               (setf point-2d.x (get-mouse-x))
                               (setf point-2d.y (get-mouse-y))))))
    (add-ecs-entity pointer)))

(defun.ps update ()
  (clear-debug-area)
  (process-input)
  (stats.update)
  (ecs-main))

(defun.ps main ()
  (let* ((scene (new (#j.THREE.Scene#)))
         (camera (init-camera screen-width screen-height))
         (renderer (new #j.THREE.WebGLRenderer#)))
    (register-default-systems scene)
    (renderer.set-size screen-width screen-height)
    ((@ ((@ document.query-selector) "#renderer") append-child) renderer.dom-element)
    (let ((light (new (#j.THREE.DirectionalLight# 0xffffff))))
      (light.position.set 0 0.7 0.7)
      (scene.add light))
    (scene.add (make-line :pos-a (list #y1333 #y500) :pos-b (list 0 #y500) :color 0x00ff00 :z 1))
    (scene.add (make-line :pos-a (list #y666 #y0) :pos-b (list #y666 #y1000) :color 0x00ff00 :z 1))
    (make-mouse-pointer)
    (refresh-entity-display)
    (setf stats (init-stats))
    (labels ((render-loop ()
               (request-animation-frame render-loop)
               (renderer.render scene camera)
               (update)))
      (render-loop))))

(defun js-main ()
  (with-use-ps-pack (:cl-shigi-simulator.static.js.2d-geometry
                     :cl-shigi-simulator.static.js.tools
                     :cl-shigi-simulator.static.js.input
                     :cl-shigi-simulator.static.js.basic-ecs
                     :this)
    (window.add-event-listener "mousemove" on-mouse-move-event)
    (window.add-event-listener "keydown" (lambda (e) (e.prevent-default)))
    (window.add-event-listener "DOMContentLoaded" main false))) 
