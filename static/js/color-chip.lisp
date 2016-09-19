(in-package :cl-user)
(defpackage cl-shigi-simulator.static.js.color-chip
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :parenscript
        :cl-web-2d-game
        :cl-shigi-simulator.static.js.tools)
  (:import-from :ps-experiment.common-macros
                :with-slots-pair))
(in-package :cl-shigi-simulator.static.js.color-chip)

(enable-ps-experiment-syntax)

(defstruct.ps+ (chip (:include ecs-component)) (nearest-id -1))

(defun.ps generate-all-color-chips ()
  (let* ((size (get-param :color-chip :size))
         (half-size (/ size 2))
         (depth (get-param :color-chip :depth)))
    (loop for y = 0 then (+ y size) while (< y screen-height)
       do (loop for x = 0 then (+ x size) while (< x screen-width)
             do (let ((chip (make-ecs-entity)))
                  (add-ecs-component-list
                   chip
                   (make-chip)
                   (make-point-2d :x (+ x half-size)
                                  :y (+ y half-size))
                   (make-model-2d :model (make-solid-rect :width size :height size
                                                          :depth depth)
                                  :offset (make-point-2d :x (* -1 half-size)
                                                         :y (* -1  half-size))))
                  (add-ecs-entity chip))))))

;; -----------------
;; TODO: Move these to a more proper package because the player also uses this.
(defun.ps+ make-shigi-part-point-pairs ()
  (let ((result '()))
    (do-tagged-ecs-entities (entity "shigi-part")
      (push (list entity (calc-global-point entity))
            result))
    result))

(defun.ps+ get-nearest-shigi-part (shigi-parts-points entity)
  (let ((min-len #y10000)
        (nearest-part nil)
        (entity-pnt (calc-global-point entity)))
    (dolist (part-pnt-pair shigi-parts-points)
      (let ((dist (calc-dist entity-pnt (cadr part-pnt-pair))))
        (when (< dist min-len)
          (setf min-len dist)
          (setf nearest-part (car part-pnt-pair)))))
    nearest-part))
;; -----------------

;; TODO: revert to defstruct.ps+
(defstruct.ps
    (color-chip-system
     (:include ecs-system
               (target-component-types '(chip model-2d))
               (process-all
                (lambda (system)
                  (let ((pair-list (make-shigi-part-point-pairs)))
                    (dolist (chip (ecs-system-target-entities system))
                      (let* ((nearest-part (get-nearest-shigi-part pair-list chip))
                             (new-id (ecs-entity-id nearest-part)))
                        (with-ecs-components ((chip-cmp chip)) chip
                          (with-slots (nearest-id) chip-cmp
                            (unless (= nearest-id new-id)
                              (setf nearest-id new-id)
                              (change-model-color
                               (get-ecs-component 'model-2d chip)
                               (get-entity-param nearest-part :color)))))))))))))
