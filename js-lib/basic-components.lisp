(in-package :cl-user)
(defpackage cl-web-2d-game.basic-components
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :parenscript)
  (:export :make-vector-2d
           :vector-2d
           :vector-2d-p
           :vector-2d-x
           :vector-2d-y

           :make-point-2d
           :point-2d
           :point-2d-p
           :point-2d-x
           :point-2d-y
           :point-2d-center
           :point-2d-angle

           :rotate-2d
           :rotate-2d-p
           :rotate-2d-speed
           :rotate-2d-angle

           :model-2d
           :model-2d-p
           :model-2d-model
           :model-2d-depth
           :model-2d-offset

           :params
           :params-table
           :get-entity-param
           :set-entity-param
           :init-entity-params

           :clone-vector
           :clone-point-2d))
(in-package :cl-web-2d-game.basic-components)

(enable-ps-experiment-syntax)

;; --- components --- ;;

(defstruct.ps+ (vector-2d (:include ecs-component)) (x 0) (y 0))
;; point-2d is mainly used as a local translation and rotation.
(defstruct.ps+ (point-2d (:include vector-2d)) (angle 0))
(defstruct.ps+ (speed-2d (:include vector-2d)))

(defstruct.ps+ (rotate-2d (:include ecs-component)) (speed 0) (angle 0) (radious 0))

(defstruct.ps+ (model-2d (:include ecs-component)) model (depth 0) (offset (make-point-2d)))

(defstruct.ps+ (params (:include ecs-component)) (table (make-hash-table)))

;; --- some functions --- ;;

;; - clone

;; TODO: rename to clone-vector-2d
(defun.ps+ clone-vector (vector)
  (with-slots (x y) vector
    (make-vector-2d :x x :y y)))

(defun.ps+ clone-point-2d (point)
  (with-slots (x y angle) point
    (make-point-2d :x x :y y :angle angle)))

;; - params

(defun.ps+ get-entity-param (entity key)
  (with-ecs-components (params) entity
    (gethash key (params-table params))))

(defun.ps+ set-entity-param (entity key new-value)
  (with-ecs-components (params) entity
    (setf (gethash key (params-table params))
          new-value)))

(defun.ps+ init-entity-params (&rest key-value-pairs)
  (unless (evenp (length key-value-pairs))
    (error "odd number of args to INIT-ENTITY-PARAMS"))
  (let* ((table (make-hash-table)))
    (labels ((rec (rest-pairs)
               (when (> (length rest-pairs) 0)
                 (setf (gethash (car rest-pairs) table)
                       (cadr rest-pairs))
                 (rec (cddr rest-pairs)))))
      (rec key-value-pairs))
    (make-params :table table)))
