(defpackage cl-shigi-simulator/static/js/player-observer
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :parenscript
        :cl-web-2d-game)
  (:export :make-player-observer)
  (:import-from :cl-shigi-simulator/static/js/player
                :check-player)
  (:import-from :cl-shigi-simulator/static/js/target
                :get-nearest-target))
(in-package :cl-shigi-simulator/static/js/player-observer)

(defstruct.ps+ nearest-part-register (part-id -1) (frame-count -1))

(defun.ps display-nearest-part (part-id frame-count)
  (let ((part (find-a-entity #'(lambda (entity)
                                 (= (ecs-entity-id entity) part-id)))))
    (labels ((pad (str len)
               ;; easy impremetation
               ((@ (+ "      " str) slice) (* len -1))))
      (add-to-event-log (+ (pad (get-entity-param part :display-name) 6) ":"
                           (pad (floor (* frame-count 1000/60)) 4) "ms ("
                           (pad frame-count 3) "F)")))))

(defun.ps+ process-observer (observer)
  (let* ((player (get-entity-param observer :player))
         (register (get-entity-param observer :nearest-part-register))
         (nearest (get-nearest-target (calc-global-point player)))
         (nearest-id (if nearest (ecs-entity-id nearest) -1)))
    (with-slots (part-id frame-count) register
      (if (= part-id nearest-id)
          (incf frame-count)
          (progn (when (>= part-id 0)
                   (display-nearest-part part-id frame-count))
                 (setf part-id nearest-id
                       frame-count 0))))))

(defun.ps+ make-player-observer (player)
  (check-player player)
  (let ((observer (make-ecs-entity)))
    (add-ecs-component-list
     observer
     (make-script-2d :func #'process-observer)
     (init-entity-params :player player
                         :nearest-part-register (make-nearest-part-register)))
    (add-ecs-entity observer)))
