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
        (r (get-param :player :body-r)))
    (add-ecs-component-list
     launcher
     (make-point-2d :x (/ shigi-screen-width 2)
                    :y (* shigi-screen-height 1/5))
     (make-model-2d :model (make-solid-circle :r r :color #x0000ff)
                    :depth 1
                    :offset (make-point-2d :x (* -1 r)
                                           :y (* -1 r))))
    (add-ecs-entity launcher)))
