(defpackage cl-shigi-simulator/static/js/test/orbiter-visualizer
  (:use :cl
        :parenscript
        :ps-experiment
        :cl-web-2d-game
        :cl-ps-ecs)
  (:export :init-visualizer)
  (:import-from :cl-shigi-simulator/static/js/test/orbiter-launcher
                :get-launcher-point
                :get-launcher-angle
                :get-rot-speed)
  (:import-from :cl-shigi-simulator/static/js/test/orbiter-target
                :get-target-point
                :get-target-angle)
  (:import-from :cl-shigi-simulator/static/js/lazer-utils
                :calc-first-lazer-speed))
(in-package :cl-shigi-simulator/static/js/test/orbiter-visualizer)

(defun.ps+ init-visualizer (&key launcher target)
  (let ((vis (make-ecs-entity))
        (line-color #xaaaaaa)
        (line-depth -11))
    (add-entity-tag vis :visualizer)
    (flet ((make-line-model (&key x y angle label)
             (make-model-2d
              :model (make-line :pos-a (list -1000 0)
                                :pos-b (list 1000 0)
                                :color line-color)
              :depth line-depth
              :offset (make-point-2d :x x :y y :angle angle)
              :label label)))
      (let ((launcher-point (get-launcher-point launcher))
            (target-point   (get-target-point   target)))
        (add-ecs-component-list
         vis
         (make-point-2d)
         (make-script-2d :func #'update-visualizer)
         (make-line-model :x (point-2d-x target-point)
                          :y (point-2d-y target-point)
                          :angle (get-target-angle target)
                          :label :target-line)
         (make-line-model :x (point-2d-x launcher-point)
                          :y (point-2d-y launcher-point)
                          :angle (- (get-launcher-angle launcher) (/ PI 2))
                          :label :launcher-line)
         (make-model-2d :model (make-wired-circle :r 0)
                        :label :start-circle)
         (init-entity-params :launcher launcher
                             :target   target))))
    (add-ecs-entity vis)))

(defun.ps+ update-visualizer (vis)
  (flet ((update-line (label pnt angle)
           (let* ((model  (find-model-2d-by-label vis label))
                  (offset (model-2d-offset model)))
             (setf (point-2d-x offset) (point-2d-x pnt)
                   (point-2d-y offset) (point-2d-y pnt)
                   (point-2d-angle offset) angle))))
    (let ((target (get-entity-param vis :target)))
      (assert target)
      (update-line :target-line
                   (get-target-point target)
                   (get-target-angle target)))
    (let ((launcher (get-entity-param vis :launcher)))
      (assert launcher)
      (update-line :launcher-line
                   (get-launcher-point launcher)
                   (- (get-launcher-angle launcher) (/ PI 2)))))
  (register-next-frame-func
   (lambda ()
     (let ((old-model (find-model-2d-by-label vis :start-circle))
           (new-model (make-circle-model vis)))
       (if old-model
           (update-model-2d vis old-model new-model)
           (add-ecs-component new-model vis))))))

(defun.ps+ calc-circle-radius-of-visualizer (vis)
  (check-entity-tags vis :visualizer)
  (let* ((target (get-entity-param vis :target))
         (launcher (get-entity-param vis :launcher))
         (rot-speed (get-rot-speed launcher))
         (speed (calc-first-lazer-speed
                 :dummy-pnt   (get-target-point target)
                 :dummy-angle (get-target-angle target)
                 :start-pnt   (get-launcher-point launcher)
                 :start-angle (get-launcher-angle launcher)
                 :rot-speed rot-speed)))
    (/ speed rot-speed)))

(defun.ps+ make-circle-model (vis)
  (check-entity-tags vis :visualizer)
  (let* ((r (calc-circle-radius-of-visualizer vis))
         (launcher (get-entity-param vis :launcher))
         (start-angle (get-launcher-angle launcher))
         (start-pnt (get-launcher-point launcher)))
    (make-model-2d
     :model (make-wired-circle :r r :color #xaaaaaa)
     :offset (make-point-2d
              :x (+ (point-2d-x start-pnt) (* r (cos start-angle)))
              :y (+ (point-2d-y start-pnt) (* r (sin start-angle))))
     :depth -1
     :label :start-circle)))
