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

(defun.ps+ set-next-ray-pnt (ray x y)
  (check-entity-tags ray :ray)
  (set-entity-param ray :next-x x :next-y y))

(defun.ps make-ray ()
  (let* ((ray (make-ecs-entity))
         (num-pnts 20)
         (init-pnt-list '())
         (r #ly10))
    (add-entity-tag ray :ray)
    (dotimes (i num-pnts)
      (push (list 0 0) init-pnt-list))
    (add-ecs-component-list
     ray
     (make-point-2d :center (make-vector-2d :x r :y r))
     (make-model-2d :model (make-lines :pnt-list init-pnt-list :color 0xff0000)
                    :depth (get-depth :mouse))
     (make-script-2d :func (lambda (entity)
                             (with-ecs-components (model-2d) entity
                               (let* ((geometry model-2d.model.geometry)
                                      (pnt-list geometry.vertices)
                                      (len (length pnt-list)))
                                 (with-slots (x y) (aref pnt-list 0)
                                   (setf x (get-entity-param entity :next-x)
                                         y (get-entity-param entity :next-y)))
                                 (add-to-monitoring-log
                                  (+ (get-entity-param entity :next-x) ":"
                                     (get-entity-param entity :next-y)))
                                 (dotimes (i (1- len))
                                   (let ((index (- len (1+ i))))
                                     (with-slots-pair (((x1 x) (y1 y)) (aref pnt-list index)
                                                       ((x0 x) (y0 y)) (aref pnt-list (1- index)))
                                       (setf x1 x0 y1 y0))))
                                 (setf geometry.vertices-need-update t)))))
     (init-entity-params :next-x 0 :next-y 0))
    (add-ecs-entity ray)))

(defun.ps+ make-controler (ray)
  (make-mouse-controler ray)
  (make-touch-controler ray))

(defun.ps+ make-mouse-controler (ray)
  (let ((ctr (make-ecs-entity)))
    (add-ecs-component-list
     ctr
     (make-script-2d :func (lambda (entity)
                             (let ((x (get-mouse-x))
                                   (y  (get-mouse-y)))
                               (unless (and (= (get-entity-param entity :prev-x) x)
                                            (= (get-entity-param entity :prev-y) y))
                                 (set-next-ray-pnt ray x y)
                                 (set-entity-param entity :prev-x x :prev-y y)))))
     (init-entity-params :prev-x 0
                         :prev-y 0))
    (add-ecs-entity ctr)))

(defun.ps+ make-touch-controler (ray)
  (let ((ctr (make-ecs-entity)))
    (add-ecs-component-list
     ctr
     (make-script-2d :func (lambda (entity)
                             (declare (ignore entity))
                             (let ((state (get-total-touch-state)))
                               (when (or (eq state :down)
                                         (eq state :down-now))
                                 (set-next-ray-pnt
                                  ray (get-total-touch-x) (get-total-touch-y)))))))
    (add-ecs-entity ctr)))

(defun.ps+ init (scene)
  (declare (ignore scene))
  (let ((ray (make-ray)))
    (make-controler ray)))

(defun.ps+ main ()
  (start-game :screen-width shigi-screen-width
              :screen-height shigi-screen-height
              :init-function #'init))

(defun js-main ()
  (with-use-ps-pack (:cl-shigi-simulator/static/js/tools
                     :cl-shigi-simulator/static/js/basic-ecs
                     :this)
    (init-input)
    (window.add-event-listener "DOMContentLoaded" main false))) 
