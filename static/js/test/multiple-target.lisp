(defpackage cl-shigi-simulator/static/js/test/multiple-target
  (:use :cl
        :parenscript
        :ps-experiment
        :cl-web-2d-game
        :cl-ps-ecs)
  (:export :init-targets
           :get-default-test-target-duration
           :set-test-target-duration)
  (:import-from :cl-shigi-simulator/static/js/lazer
                :get-lazer-tag)
  (:import-from :cl-shigi-simulator/static/js/target
                :target-component
                :make-target-component
                :target-component-num-lazer-to-destroy
                :get-target-tag)
  (:import-from :cl-shigi-simulator/static/js/tools
                :get-depth
                :get-param))
(in-package :cl-shigi-simulator/static/js/test/multiple-target)

(defmacro.ps+ get-my-param (&rest rest)
  `(get-param :test :multiple ,@rest))

(defun.ps+ init-targets ()
  (add-a-target :x #lx200 :y #ly300)
  (add-a-target :x #lx400 :y #ly300))

(defun.ps+ add-a-target (&key x y)
  (let ((target (make-ecs-entity))
        (r (get-my-param :target-r)))
    (add-entity-tag target (get-target-tag) :test-target)
    (add-ecs-component-list
     target
     (make-target-component
      :num-lazer-to-destroy (get-default-test-target-duration))
     (make-point-2d :x x :y y)
     (make-model-2d :model (make-wired-circle :r r :color #x000000)
                    :depth (get-depth :enemy))
     (make-physic-circle :r r
                         :target-tags (list (get-lazer-tag))))
    (add-ecs-entity target)))

(defun.ps+ get-default-test-target-duration () 1)

(defun.ps+ set-test-target-duration (val)
  (do-tagged-ecs-entities (target :test-target)
    (setf (target-component-num-lazer-to-destroy
           (get-ecs-component 'target-component target))
          val)))
