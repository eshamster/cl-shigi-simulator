(in-package :cl-user)
(defpackage cl-shigi-simulator.static.js.shigi
  (:use :cl
        :cl-ppcre
        :parenscript)
  (:import-from :ps-experiment
                :setf-with
                :defun.ps
                :defvar.ps
                :with-use-ps-pack) 
  (:import-from :cl-ps-ecs
                :with-ecs-components))
(in-package :cl-shigi-simulator.static.js.shigi)

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
  (let* ((fov 60)
         (rad-fov (/ (* fov PI) 180))
         (aspect (/ width height))
         (z (abs (* (/ height 2)
                    (/ 1 (tan (/ rad-fov 2))))))
         (camera (new (#j.THREE.PerspectiveCamera# fov aspect
                                                   (/ z 2) (* z 2)))))
    (camera.position.set (/ width 2) (/ height 2) z)
    camera))

(defun.ps make-sample-move-entities ()
  (let ((parent (make-ecs-entity))
        (child (make-ecs-entity)))
    ;; make parent
    (add-ecs-component (make-model-2d :model (make-wired-rect :width #y640 :height #y480
                                                              :color 0xff00ff)
                                      :depth 1)
                       parent)
    (add-ecs-component (make-point-2d) parent)
    (add-ecs-component (make-speed-2d :x #y0.8 :y #y0.6) parent)
    (add-ecs-component (make-script-2d :func (lambda (entity)
                                               (with-ecs-components (point-2d) entity
                                                 (when (is-key-down-now :b)
                                                   (setf point-2d.x 0)
                                                   (setf point-2d.y 0)))))
                       parent)
    ;; make child
    (add-ecs-component (make-model-2d :model (make-solid-rect :width #y30 :height #y50
                                                              :color 0x00ff00)
                                      :depth 1.1)
                       child)
    (add-ecs-component (make-point-2d :center (make-vector-2d :x #y15 :y #y25)) child) 
    (add-ecs-component (make-speed-2d :x #y0.4) child)
    (add-ecs-component (make-rotate-2d :speed (/ PI 120)) child)
    ;; register
    (add-ecs-entity parent)
    (add-ecs-entity child parent)))

(defun.ps make-sample-rotate-entities ()
  (let ((parent (make-ecs-entity))
        (parent-r #y80)
        (child (make-ecs-entity))
        (child-r #y40)
        (child-dist #y200)
        (gchild (make-ecs-entity))
        (gchild-r #y20)
        (gchild-dist #y80))
    ;; make parent
    (add-ecs-component (make-model-2d :model (make-solid-regular-polygon :r parent-r :n 6 :color 0x00ffff)
                                      :depth 0.5)
                       parent)
    (add-ecs-component (make-point-2d :x #y600 :y #y450 :center (make-vector-2d :x parent-r :y parent-r)) parent)
    (add-ecs-component (make-rotate-2d :speed (/ PI 120)) parent)
    ;; make child
    (add-ecs-component (make-model-2d :model (make-wired-regular-polygon :r child-r :n 6 :color 0x00ffff)
                                      :depth 0.5)
                       child)
    (add-ecs-component (make-point-2d :x child-dist :center (make-vector-2d :x child-r :y child-r)) child)
    (add-ecs-component (make-rotate-2d :speed (* -1 (/ PI 60))) child)
    (add-ecs-component (make-rotate-2d :speed (/ PI 360) :rot-offset (make-vector-2d :x child-dist)) child)
    ;; make grandchild
    (add-ecs-component (make-model-2d :model (make-wired-regular-polygon :r gchild-r :n 6 :color 0x00ffff)
                                      :depth 0.5)
                       gchild)
    (add-ecs-component (make-point-2d :x gchild-dist :center (make-vector-2d :x gchild-r :y gchild-r)) gchild)
    (add-ecs-component (make-rotate-2d :speed (* -1 (/ PI 300)) :rot-offset (make-vector-2d :x gchild-dist)) gchild)
    ;; register
    (add-ecs-entity parent)
    (add-ecs-entity child parent)
    (add-ecs-entity gchild child)))

(defun.ps make-mouse-pointer ()
  (let ((pointer (make-ecs-entity))
        (r 5))
    (add-ecs-component (make-point-2d :center (make-vector-2d :x r :y r)) pointer)
    (add-ecs-component (make-model-2d :model (make-wired-regular-polygon :n 60 :color 0xff0000
                                                                         :r r)
                                      :depth 1)
                       pointer)
    (add-ecs-component (make-script-2d :func (lambda (entity)
                                               (with-ecs-components (point-2d) entity
                                                 (setf point-2d.x (get-mouse-x))
                                                 (setf point-2d.y (get-mouse-y)))))
                       pointer)
    (add-ecs-entity pointer)))

(defun.ps make-sample-entities ()
  (make-sample-move-entities)
  (make-sample-rotate-entities)
  (make-mouse-pointer))

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
    (scene.add (make-line :pos-a '(0 0) :pos-b (list #y1333 #y1000) :color 0x00ff00 :z 1))
    (make-sample-entities)
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
                     :cl-shigi-simulator.static.js.player
                     :cl-shigi-simulator.static.js.basic-ecs
                     :this)
    (window.add-event-listener "mousemove" on-mouse-move-event)
    (window.add-event-listener "DOMContentLoaded" main false))) 
