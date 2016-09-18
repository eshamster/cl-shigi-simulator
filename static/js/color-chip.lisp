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

(defstruct.ps+ (chip (:include ecs-component)))

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
               (target-component-types '(chip point-2d))
               (process-all
                (lambda (system)
                  (let ((pair-list (make-shigi-part-point-pairs)))
                    ;; [WIP]
                    (append-debug-text
                     (ecs-entity-id
                      (get-nearest-shigi-part pair-list
                                              (find-a-entity-by-tag "mouse"))))))))))
