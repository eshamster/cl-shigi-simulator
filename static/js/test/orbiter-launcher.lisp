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
           :find-launcher)
  (:import-from :cl-shigi-simulator/static/js/tools
                :get-param
                :shigi-screen-width
                :shigi-screen-height))
(in-package :cl-shigi-simulator/static/js/test/orbiter-launcher)

(defun.ps+ init-launcher ()
  (let ((launcher (make-ecs-entity))
        (r (get-param :player :body-r))
        (angle (* 20 (/ PI 180))))
    (add-entity-tag launcher :launcher)
    (add-ecs-component-list
     launcher
     (make-point-2d :x (/ shigi-screen-width 2)
                    :y (* shigi-screen-height 1/5))
     (make-model-2d :model (make-solid-circle :r r :color #x0000ff)
                    :depth 1)
     (init-entity-params :start-angle angle))
    (add-ecs-entity launcher)))

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
