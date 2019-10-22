(defpackage cl-shigi-simulator/static/js/test/ray
  (:use :cl
        :cl-ppcre
        :parenscript
        :ps-experiment
        :cl-web-2d-game
        :cl-shigi-simulator/static/js/tools
        :cl-ps-ecs)
  (:import-from :ps-experiment/common-macros
                :with-slots-pair))
(in-package :cl-shigi-simulator/static/js/test/ray)

(defun.ps make-mouse-pointer ()
  (let* ((pointer (make-ecs-entity))
         (num-pnts 20)
         (init-pnt-list '())
         (r #y10))
    (dotimes (i num-pnts)
      (push (list (- 100 (* 10 i)) 0) init-pnt-list))
    (add-ecs-component-list
     pointer
     (make-point-2d :center (make-vector-2d :x r :y r))
     (make-model-2d :model (make-lines :pnt-list init-pnt-list :color 0xff0000)
                    :depth (get-depth :mouse))
     (make-script-2d :func (lambda (entity)
                             (with-ecs-components (model-2d) entity
                               (let* ((geometry model-2d.model.geometry)
                                      (pnt-list geometry.vertices)
                                      (len (length pnt-list)))
                                 (with-slots (x y) (aref pnt-list 0)
                                   (setf x (get-mouse-x)
                                         y (get-mouse-y)))
                                 (dotimes (i (1- len))
                                   (let ((index (- len (1+ i))))
                                     (with-slots-pair (((x1 x) (y1 y)) (aref pnt-list index)
                                                       ((x0 x) (y0 y)) (aref pnt-list (1- index)))
                                       (setf x1 x0 y1 y0))))
                                 (setf geometry.vertices-need-update t))))))
    (add-ecs-entity pointer)))

(defun.ps init (scene)
  (scene.add (make-line :pos-a (list #y1333 #y500) :pos-b (list 0 #y500) :color 0x00ff00))
  (scene.add (make-line :pos-a (list #y666 #y0) :pos-b (list #y666 #y1000) :color 0x00ff00))
  (make-mouse-pointer))

(defun.ps main ()
  (start-game :screen-width shigi-screen-width
              :screen-height shigi-screen-height
              :init-function init))

(defun js-main ()
  (with-use-ps-pack (:cl-shigi-simulator/static/js/tools
                     :cl-shigi-simulator/static/js/basic-ecs
                     :this)
    (init-input)
    (window.add-event-listener "DOMContentLoaded" main false))) 
