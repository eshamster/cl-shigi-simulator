(in-package :cl-user)
(defpackage cl-shigi-simulator.static.js.collision
  (:use :cl
        :parenscript
        :cl-ps-ecs
        :ps-experiment
        :cl-shigi-simulator.static.js.basic-components)
  (:export :process-collision
           :collision-system
           :make-collision-system))
(in-package :cl-shigi-simulator.static.js.collision)

;; --- components --- ;;

(defstruct.ps+ (physic-2d (:include ecs-component))
  kind
  (offset (make-vector-2d))
  (on-collision (lambda (mine target) (declare (ignore mine target)) nil)))

(defstruct.ps+ (physic-circle (:include physic-2d (kind :circle))) (r 0))

;; --- utils --- ;;
;; TODO: move this definition to more appropriate package
(defmacro.ps+ with-slots-pair (pair &body body)
  (unless (evenp (length pair))
    (error "with-slots-pair needs an even number length list as a first argument"))
  (labels ((rec (rest-pair)
             (if rest-pair
                 `((with-slots ,(car rest-pair) ,(cadr rest-pair)
                     ,@(rec (cddr rest-pair))))
                 body)))
    (car (rec pair))))

;; --- basic funcions --- ;;

;; c: Circle
;; t: Triangle
;; r: Rectangle

;; c to c
(defun.ps+ col-cc (x1 y1 r1 x2 y2 r2)
  (<= (+ (expt (- x1 x2) 2)
         (expt (- y1 y2) 2))
      (expt (+ r1 r2) 2)))

;; Note: the offset parameters is not well-tested
(defun.ps+ col-cc-vec (point1 offset1 r1 point2 offset2 r2)
  (with-slots-pair (((x1 x) (y1 y)) point1
                    ((ox1 x) (oy1 y)) offset1
                    ((x2 x) (y2 y)) point2
                    ((ox2 x) (oy2 y)) offset2)
    (col-cc (+ x1 ox1) (+ y1 ox1) r1
            (+ x2 ox2) (+ y2 oy2) r2)))

(defun.ps+ col-cc-physic (circle1 point1 circle2 point2)
  (check-type circle1 physic-circle)
  (check-type circle2 physic-circle)
  (with-slots-pair (((r1 r) (offset1 offset)) circle1
                    ((r2 r) (offset2 offset)) circle2)
    (col-cc-vec point1 offset1 r1
                point2 offset2 r2)))

;; --- auxiliary functions --- ;;

(defun.ps+ process-collision (entity1 entity2)
  (with-ecs-components ((ph1 physic-2d) (pnt1 point-2d)) entity1
    (with-ecs-components ((ph2 physic-2d) (pnt2 point-2d)) entity2
      (when (cond ((and (eq (physic-2d-kind ph1) :circle)
                        (eq (physic-2d-kind ph2) :circle))
                   (col-cc-physic ph1 pnt1 ph2 pnt2))
                  (t (error "not recognized physical type")))
        (with-slots-pair (((event1 on-collision)) ph1
                          ((event2 on-collision)) ph2)
          (funcall event1 entity1 entity2)
          (funcall event2 entity2 entity1))))))

;; --- system --- ;;

(defstruct.ps+
    (collision-system
     (:include ecs-system
               (target-component-types '(point-2d physic-2d))
               (process-all
                (lambda (system)
                  (with-slots ((entities target-entities)) system
                    (let ((length (length entities)))
                      (loop for outer-idx from 0 below (1- length) do
                           (loop for inner-idx from (1+ outer-idx) below length do
                                (process-collision (aref entities outer-idx)
                                                   (aref entities inner-idx)))))))))))
