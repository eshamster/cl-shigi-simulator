(defpackage cl-shigi-simulator/static/js/test/multiple-observer
  (:use :cl
        :parenscript
        :ps-experiment
        :cl-web-2d-game
        :cl-ps-ecs)
  (:export :make-observer)
  (:import-from :cl-shigi-simulator/static/js/lazer
                :get-lazer-num)
  (:import-from :cl-shigi-simulator/static/js/player
                :check-player)
  (:import-from :cl-shigi-simulator/static/js/target
                :sort-targets-by-dist)
  (:import-from :cl-shigi-simulator/static/js/tools
                :get-param
                :get-depth))
(in-package :cl-shigi-simulator/static/js/test/multiple-observer)

(defun.ps+ make-observer (player)
  (check-player player)
  (let ((numbers (list))
        (num (get-lazer-num)))
    (dotimes (i num)
      (push (make-number-entity (- num i)) ; from 1 to num
            numbers))
    (let ((observer (make-ecs-entity)))
      (add-ecs-component-list
       observer
       (make-script-2d :func #'process-observer)
       (init-entity-params :player player
                           :numbers numbers))
      (add-ecs-entity observer))))

(defun.ps+ process-observer (observer)
  (let* ((numbers (get-entity-param observer :numbers))
         (player (get-entity-param observer :player))
         (sorted-targets (sort-targets-by-dist (calc-global-point player)))
         (parent-point (calc-global-point (get-default-ecs-entity-parent))))
    (assert numbers)
    (labels ((rec (rest-numbers rest-targets)
               (let ((num-entity (car rest-numbers))
                     (target (car rest-targets)))
                 (when num-entity
                   (set-number-entity-point
                    num-entity
                    (if target
                        (calc-global-point target)
                        (make-point-2d :x #lx-1000 :y #ly-1000))
                    parent-point)
                   (rec (cdr rest-numbers) (cdr rest-targets))))))
      (rec numbers sorted-targets))))

(defun.ps+ make-number-entity (number)
  (let* ((font-size (get-param :test :multiple :font-size))
         (text-area (make-text-area :font-size font-size
                                    :text-align :center
                                    :x #lx-1000 :y #ly-1000
                                    :depth (+ (get-depth :enemy) 5))))
    (add-text-to-area text-area :text number :color #x000000)
    (add-ecs-entity text-area)))

(defun.ps+ set-number-entity-point (number-entity global-pnt base-pnt)
  (let ((local-pnt (transformf-point-inverse (clone-point-2d global-pnt) base-pnt))
        (font-size (get-param :test :multiple :font-size))
        (pnt (get-ecs-component 'point-2d number-entity)))
    (assert pnt)
    (setf (point-2d-x pnt) (point-2d-x local-pnt)
          (point-2d-y pnt) (+ (point-2d-y local-pnt)
                              (/ font-size 2)))))
