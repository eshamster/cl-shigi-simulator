(defpackage cl-shigi-simulator/static/js/test/orbiter-target
  (:use :cl
        :parenscript
        :ps-experiment
        :cl-web-2d-game
        :cl-ps-ecs)
  (:export :init-dummy-target
           :get-target-point
           :set-target-point
           :get-target-angle
           :set-target-angle
           :find-target)
  (:import-from :cl-shigi-simulator/static/js/tools
                :get-depth
                :get-param
                :shigi-screen-width
                :shigi-screen-height))
(in-package :cl-shigi-simulator/static/js/test/orbiter-target)

(defun.ps+ init-dummy-target ()
  (let ((target (make-ecs-entity))
        (half-len 25)
        (angle (* PI 1/3)))
    (add-entity-tag target :target)
    (flet ((make-line-model (x1 y1 x2 y2)
             (make-model-2d
              :model (make-line :pos-a (list x1 y1)
                                :pos-b (list x2 y2)
                                :color #x888888)
              :depth (get-depth :enemy))))
      (add-ecs-component-list
       target
       (make-point-2d :x (* shigi-screen-width 600/1000)
                      :y (* shigi-screen-height 400/1000))
       (make-line-model (* -1 half-len) 0 half-len 0)
       (make-line-model 0 (* -1 half-len) 0 half-len)
       (init-entity-params :angle angle)))
    (add-ecs-entity target)))

;; XXX: These assume that local point is same to global point.
(defun.ps+ get-target-point (&optional (target (find-target)))
  (check-entity-tags target :target)
  (get-ecs-component 'point-2d target))

(defun.ps+ set-target-point (x y &optional (target (find-target)))
  (check-entity-tags target :target)
  (with-ecs-components (point-2d) target
    (setf (point-2d-x point-2d) x
          (point-2d-y point-2d) y)))

(defun.ps+ get-target-angle (&optional (target (find-target)))
  (check-entity-tags target :target)
  (get-entity-param target :angle))

(defun.ps+ set-target-angle (angle &optional (target (find-target)))
  (check-entity-tags target :target)
  (set-entity-param target :angle angle))

(defun.ps+ find-target ()
  (let ((target (find-a-entity-by-tag :target)))
    (assert target)
    target))
