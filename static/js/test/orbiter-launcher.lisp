(defpackage cl-shigi-simulator/static/js/test/orbiter-launcher
  (:use :cl
        :parenscript
        :ps-experiment
        :cl-web-2d-game
        :cl-ps-ecs)
  (:export :init-launcher
           :get-launcher-point
           :get-launcher-angle
           :set-launcher-angle
           :find-launcher
           :shot-lazer)
  (:import-from :cl-shigi-simulator/static/js/test/orbiter-target
                :get-target-point
                :get-target-angle)
  (:import-from :cl-shigi-simulator/static/js/lazer-utils
                :calc-first-lazer-speed)
  (:import-from :cl-shigi-simulator/static/js/lazer
                :make-a-lazer)
  (:import-from :cl-shigi-simulator/static/js/tools
                :get-param
                :shigi-screen-width
                :shigi-screen-height))
(in-package :cl-shigi-simulator/static/js/test/orbiter-launcher)

(defun.ps+ init-launcher (dummy-target)
  (check-entity-tags dummy-target :target)
  (let ((launcher (make-ecs-entity))
        (r (get-param :player :body-r))
        (angle (* 20 (/ PI 180))))
    (add-entity-tag launcher :launcher)
    (add-ecs-component-list
     launcher
     (make-point-2d :x (* shigi-screen-width 300/1000)
                    :y (* shigi-screen-height 300/1000))
     (make-model-2d :model (make-solid-circle :r r :color #x0000ff)
                    :depth 1)
     (make-script-2d :func (lambda (entity)
                             (when (get-entity-param entity :lazer-triggered-p)
                               (set-entity-param entity :lazer-triggered-p nil)
                               (make-my-lazer entity))))
     (init-entity-params :lazer-triggered-p nil
                         :start-angle angle
                         :target dummy-target))
    (add-ecs-entity launcher)))

(defun.ps+ shot-lazer (&optional (launcher (find-launcher)))
  (check-entity-tags launcher :launcher)
  (set-entity-param launcher :lazer-triggered-p t))

(defun.ps+ make-my-lazer (launcher)
  (let* ((target (get-entity-param launcher :target))
         (rot-speed (get-param :lazer :rot-speed))
         (dummy-pnt   (get-target-point target))
         (dummy-angle (get-target-angle target))
         (start-pnt   (get-launcher-point launcher))
         (start-angle (get-launcher-angle launcher))
         (speed (calc-first-lazer-speed
                 :dummy-pnt   dummy-pnt
                 :dummy-angle dummy-angle
                 :start-pnt   start-pnt
                 :start-angle start-angle
                 :rot-speed rot-speed)))
    (add-ecs-entity
     (make-a-lazer :rightp t
                   :start-point start-pnt
                   :first-angle start-angle
                   :first-speed speed
                   :first-offset (make-point-2d)
                   :dummy-target-offset (sub-vector-2d dummy-pnt start-pnt)))))

(defun.ps+ get-launcher-point (&optional (launcher (find-launcher)))
  (check-entity-tags launcher :launcher)
  (calc-global-point launcher))

(defun.ps+ get-launcher-angle (&optional (launcher (find-launcher)))
  "Get relative angle to -PI/2"
  (check-entity-tags launcher :launcher)
  (get-entity-param launcher :start-angle))

(defun.ps+ set-launcher-angle (angle &optional (launcher (find-launcher)))
  "Set relative angle to -PI/2"
  (check-entity-tags launcher :launcher)
  (set-entity-param launcher :start-angle angle))

(defun.ps+ find-launcher ()
  (let ((res (find-a-entity-by-tag :launcher)))
    (assert res)
    res))
