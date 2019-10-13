(defpackage cl-shigi-simulator/static/js/test/orbiter-visualizer
  (:use :cl
        :parenscript
        :ps-experiment
        :cl-web-2d-game
        :cl-ps-ecs)
  (:export :init-visualizer)
  (:import-from :cl-shigi-simulator/static/js/test/orbiter-launcher
                :get-launcher-point
                :get-launcher-angle)
  (:import-from :cl-shigi-simulator/static/js/test/orbiter-target
                :get-target-point
                :get-target-angle))
(in-package :cl-shigi-simulator/static/js/test/orbiter-visualizer)

(defun.ps+ init-visualizer (&key launcher target)
  (let ((viz (make-ecs-entity))
        (line-color #xaaaaaa)
        (line-depth -11))
    (add-entity-tag viz :vizualizer)
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
         viz
         (make-point-2d)
         (make-script-2d :func #'update-vizualizer)
         (make-line-model :x (point-2d-x target-point)
                          :y (point-2d-y target-point)
                          :angle (get-target-angle target)
                          :label :target-line)
         (make-line-model :x (point-2d-x launcher-point)
                          :y (point-2d-y launcher-point)
                          :angle (- (get-launcher-angle launcher) (/ PI 2))
                          :label :launcher-line)
         (init-entity-params :launcher launcher
                             :target   target))))
    (add-ecs-entity viz)))

(defun.ps+ update-vizualizer (viz)
  (flet ((update-line (label pnt angle)
           (let* ((model  (find-model-2d-by-label viz label))
                  (offset (model-2d-offset model)))
             (setf (point-2d-x offset) (point-2d-x pnt)
                   (point-2d-y offset) (point-2d-y pnt)
                   (point-2d-angle offset) angle))))
    (let ((target (get-entity-param viz :target)))
      (assert target)
      (update-line :target-line
                   (get-target-point target)
                   (get-target-angle target)))
    (let ((launcher (get-entity-param viz :launcher)))
      (assert launcher)
      (add-to-monitoring-log
       (+ (point-2d-x (get-launcher-point launcher))))
      (update-line :launcher-line
                   (get-launcher-point launcher)
                   (- (get-launcher-angle launcher) (/ PI 2))))))
