(in-package :cl-user)
(defpackage cl-shigi-simulator.static.js.shigi
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :parenscript
        :cl-shigi-simulator.static.js.tools))
(in-package :cl-shigi-simulator.static.js.shigi)
(enable-ps-experiment-syntax)

(defun.ps make-shigi-bits ()
  (let ((result '())
        (num-bit 4)
        (rot-speed (get-param :shigi :bit :rot-speed))
        (r (get-param :shigi :bit :r))
        (dist (get-param :shigi :bit :dist)))
    (dotimes (i num-bit)
      (let* ((bit (make-ecs-entity))
             (angle (* 2 PI i (/ 1 num-bit))) 
             (center (make-vector-2d :x (- (* dist (cos angle)) r)
                                     :y (- (* dist (sin angle)) r))))
        (add-ecs-component-list
         bit
         (make-model-2d :model (make-wired-regular-polygon :r r :n 100 :color 0x44ff44)
                        :depth (get-param :player :depth))
         (make-point-2d :x center.x :y center.y)))
        (push bit result)))
    result))

(defun.ps make-shigi-center ()
  (let ((center (make-ecs-entity)))
    (add-ecs-component-list
     center
     (make-point-2d :x #y(* 500 4/3) :y #y800))
    center))

(defun.ps make-shigi ()
  (let ((center (make-shigi-center))
        (bit-list (make-shigi-bits)))
    (add-ecs-entity center)
    (dolist (bit bit-list)
      (add-ecs-entity bit center))))
