(defpackage cl-shigi-simulator/static/js/test/multiple-target
  (:use :cl
        :parenscript
        :ps-experiment
        :cl-web-2d-game
        :cl-ps-ecs)
  (:export :init-targets)
  (:import-from :cl-shigi-simulator/static/js/lazer
                :get-lazer-tag)
  (:import-from :cl-shigi-simulator/static/js/target
                :make-target-component
                :get-target-tag)
  (:import-from :cl-shigi-simulator/static/js/tools
                :get-depth
                :get-param))
(in-package :cl-shigi-simulator/static/js/test/multiple-target)

(defmacro.ps+ get-my-param (&rest rest)
  `(get-param :test :multiple ,@rest))

(defun.ps+ init-targets ()
  (add-a-target :x #lx200 :y #ly300))

(defun.ps+ add-a-target (&key x y)
  (let ((target (make-ecs-entity))
        (r (get-my-param :target-r)))
    (add-entity-tag target (get-target-tag))
    (add-ecs-component-list
     target
     (make-target-component :num-lazer-to-destroy 1)
     (make-point-2d :x x :y y)
     (make-model-2d :model (make-wired-circle :r r :color #x000000)
                    :depth (get-depth :enemy))
     (make-physic-circle :r r
                         :target-tags (list (get-lazer-tag))))
    (add-ecs-entity target)))
