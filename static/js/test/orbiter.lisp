(defpackage cl-shigi-simulator/static/js/test/orbiter
  (:use :cl
        :cl-ppcre
        :parenscript
        :ps-experiment
        :cl-web-2d-game
        :cl-shigi-simulator/static/js/tools
        :cl-ps-ecs)
  (:import-from :cl-shigi-simulator/static/js/test/orbiter-launcher
                :init-launcher
                :shot-lazer
                :get-launcher-angle
                :set-launcher-angle
                :get-rot-speed
                :set-rot-speed)
  (:import-from :cl-shigi-simulator/static/js/test/orbiter-target
                :init-dummy-target
                :get-target-point
                :set-target-point
                :get-target-angle
                :set-target-angle)
  (:import-from :cl-shigi-simulator/static/js/test/orbiter-visualizer
                :init-visualizer))
(in-package :cl-shigi-simulator/static/js/test/orbiter)

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

(defun.ps+ init-controler ()
  (init-mouse-controler)
  (init-touch-controler))

(defun.ps+ init-mouse-controler ()
  (let ((ctr (make-ecs-entity)))
    (add-ecs-component-list
     ctr
     (make-script-2d
      :func (lambda (entity)
              (declare (ignore entity))
              ;; Control target
              (when (eq (get-left-mouse-state) :down)
                (let ((x (get-mouse-x))
                      (y (get-mouse-y)))
                  (when (point-in-screen-p x y)
                    (set-target-point x y))))
              (let ((diff-angle (* PI 1/60)))
                (when (> (get-mouse-wheel-delta-y) 0)
                  (set-target-angle (- (get-target-angle) diff-angle)))
                (when (< (get-mouse-wheel-delta-y) 0)
                  (set-target-angle (+ (get-target-angle) diff-angle))))
              ;; Control lazer
              (when (key-down-now-p :c)
                (shot-lazer)))))
    (add-ecs-entity ctr)))

(defun.ps+ init-touch-controler ()
  (let ((ctr (make-ecs-entity)))
    (add-ecs-component-list
     ctr
     (make-script-2d
      :func (lambda (entity)
              (declare (ignore entity))
              (let ((state (get-total-touch-state)))
                ;; Control target
                (when (or (eq state :down-now)
                          (eq state :down))
                  (set-target-point (get-total-touch-x)
                                    (get-total-touch-y)))
                ;; Control lazer
                (when (eq state :up-now)
                  (shot-lazer))))))
    (add-ecs-entity ctr)))

(defun.ps+ point-in-screen-p (x y)
  (and (>= x 0) (<= x shigi-screen-width)
       (>= y 0) (<= y shigi-screen-height)))

(defun.ps+ init-background ()
  (let ((bg (make-ecs-entity)))
    (add-ecs-component-list
     bg
     (make-point-2d)
     (make-model-2d :model (make-solid-rect :width shigi-screen-width
                                            :height shigi-screen-height
                                            :color #xeeeeee)
                    :depth (get-depth :background)))
    (add-ecs-entity bg)))

(defun.ps+ init-panel (launcher)
  (add-panel-number
   "Start Angle" (* (get-launcher-angle launcher) (/ 180 PI))
   :min -45
   :max 90
   :on-change (lambda (val)
                (set-launcher-angle (* val (/ PI 180)) launcher)))
  (let ((default-rs (get-rot-speed launcher)))
    (add-panel-number
     "Rotation Speed" default-rs
     :min (/ default-rs 3)
     :max (* default-rs 3)
     :step (/ default-rs 100)
     :on-change (lambda (val)
                  (set-rot-speed val launcher)))))

(defun.ps+ init-description ()
  (add-to-event-log "Wheel up/down to change target angle")
  (add-to-event-log "Mouse drag to set target point")
  (add-to-event-log "Press 'C' key to shot lazer"))

(defun.ps+ init (scene)
  (declare (ignore scene))
  (setf-collider-model-enable nil)
  (init-mouse-pointer)
  (init-background)
  (let* ((target (init-dummy-target))
         (launcher (init-launcher target)))
    (init-visualizer :launcher launcher
                     :target target)
    (init-panel launcher))
  (init-controler)
  (init-description))

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
