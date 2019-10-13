(defpackage cl-shigi-simulator/static/js/test/orbiter-launcher
  (:use :cl
        :parenscript
        :ps-experiment
        :cl-web-2d-game
        :cl-ps-ecs)
  (:export :init-launcher)
  (:import-from :cl-shigi-simulator/static/js/tools
                :get-param
                :shigi-screen-width
                :shigi-screen-height))
(in-package :cl-shigi-simulator/static/js/test/orbiter-launcher)

(defun.ps+ init-launcher ()
  (let ((launcher (make-ecs-entity))
        (r (get-param :player :body-r))
        (angle (- (* 20 (/ PI 180))
                  (/ PI 2))))
    (add-entity-tag launcher :launcher)
    (add-ecs-component-list
     launcher
     (make-point-2d :x (/ shigi-screen-width 2)
                    :y (* shigi-screen-height 1/5))
     (make-model-2d :model (make-solid-circle :r r :color #x0000ff)
                    :depth 1)
     (make-model-2d :model (make-line :pos-a (list -1000 0)
                                      :pos-b (list 1000 0)
                                      :color #xaaaaaa)
                    :depth -1
                    :offset (make-point-2d :angle angle)
                    :label :start-angle))
    (add-ecs-entity launcher)))

(defun.ps+ get-launcher-angle (&optional (launcher (find-launcher)))
  "Get relative angle to -PI/2"
  (let ((model (find-model-2d-by-label launcher :start-angle)))
    (assert model)
    (point-2d-angle (model-2d-offset model))))

(defun.ps+ set-launcher-angle (angle &optional (launcher (find-launcher)))
  "Set relative angle to -PI/2"
  (let ((model (find-model-2d-by-label launcher :start-angle)))
    (assert model)
    (setf (point-2d-angle (model-2d-offset model))
          angle)))

(defun.ps+ find-launcher ()
  (let ((res (find-a-entity-by-tag :launcher)))
    (assert res)
    res))
