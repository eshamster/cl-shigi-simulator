(defpackage cl-shigi-simulator/static/js/target
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :parenscript
        :cl-web-2d-game)
  (:export :target-component
           :make-target-component
           :target-component-num-lazer-to-destroy
           :target-component-enable
           :target-enable-p
           :get-target-tag
           :get-target-pnt-pairs
           :get-nearest-target
           :sort-targets-by-dist))
(in-package :cl-shigi-simulator/static/js/target)

(defstruct.ps+ (target-component (:include ecs-component))
    (num-lazer-to-destroy 12)
  (enable t))

(defun.ps+ get-target-tag ()
  :target)

(defun.ps+ get-target-pnt-pairs ()
  (let ((pairs (list)))
    (do-tagged-ecs-entities (target (get-target-tag))
      (check-target target)
      (when (target-component-enable
             (get-ecs-component 'target-component target))
        (push (list target (calc-global-point target))
              pairs)))
    pairs))

(defun.ps+ get-nearest-target (center-pnt &optional (target-pnt-pairs (get-target-pnt-pairs)))
  (let ((nearest-target nil)
        (nearest-dist -1))
    (dolist (pair target-pnt-pairs)
      (let ((target (car pair))
            (pnt (cadr pair)))
        ;; (check-target target) ; Prioritize speed over safety
        (let ((dist (calc-dist-p2 center-pnt pnt)))
          (when (or (null nearest-target)
                    (< dist nearest-dist))
            (setf nearest-dist dist
                  nearest-target target)))))
    nearest-target))

(defun.ps+ sort-targets-by-dist (center-pnt &optional (target-pnt-pairs (get-target-pnt-pairs)))
  (let ((target-dist-pairs (list)))
    (do-tagged-ecs-entities (entity (get-target-tag))
      (check-target entity)
      (push (list entity
                  (calc-dist-p2 center-pnt (calc-global-point entity)))
            target-dist-pairs))
    (mapcar (lambda (pair) (car pair))
            (sort target-dist-pairs
                  (lambda (a b)
                    (< (cadr a) (cadr b)))))))

(defun.ps+ target-enable-p (target)
  (unless target
    (return-from target-enable-p nil))
  (check-target target)
  (target-component-enable (get-ecs-component 'target-component target)))

(defun.ps+ check-target (target)
  (check-entity-tags target (get-target-tag))
  (assert (get-ecs-component 'point-2d target))
  (assert (get-ecs-component 'target-component target)))
