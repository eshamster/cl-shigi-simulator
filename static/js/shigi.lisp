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
             (center (make-vector-2d :x r :y r))
             (point (make-vector-2d :x (* dist (cos angle))
                                    :y (* dist (sin angle)))))
        (add-ecs-component-list
         bit
         (make-model-2d :model (make-wired-regular-polygon :r r :n 100 :color 0x44ff44)
                        :depth (get-param :player :depth))
         (make-point-2d :x point.x :y point.y :center center)
         (make-rotate-2d :speed rot-speed
                         :rot-offset (make-vector-2d :x point.x
                                                     :y point.y))))
      (push bit result))
    result))

(defun.ps make-shigi-bodies ()
  (let ((result '())
        (pnt-list '((#.#y0 #.#y76.8) (#.#y76.8 #.#y115.2)
                    (#.#y92.16 #.#y-57.6) (#.#y0 #.#y-144))))
    (labels ((reverse-list-by-x (pnt-list)
               (let ((result '()))
                 (dolist (pnt pnt-list)
                   (push (list (* (car pnt) -1) (cadr pnt)) result))
                 result)))
      (dotimes (i 2)
        (let ((body (make-ecs-entity)))
          (add-ecs-component-list
           body
           (make-model-2d :model (make-wired-polygon
                                  :pnt-list (if (= i 0)
                                                pnt-list
                                                (reverse-list-by-x pnt-list))
                                  :color 0x44ff44)
                          :depth (get-param :shigi :depth))
           (make-point-2d :x 0 :y 0))
          
          (push body result))))
    result))

(defun.ps make-shigi-center ()
  (let ((center (make-ecs-entity))
        (point (make-vector-2d :x (* dist (cos angle))
                               :y (* dist (sin angle)))))
    (add-ecs-component-list
     center
     (make-model-2d :model)
     (make-point-2d :x #y(* 500 4/3) :y #y800))
    center))

(defun.ps make-shigi ()
  (let ((center (make-shigi-center))
        (bodies (make-shigi-bodies))
        (bit-list (make-shigi-bits)))
    (add-ecs-entity center)
    (dolist (body bodies)
      (add-ecs-entity body center))
    (dolist (bit bit-list)
      (add-ecs-entity bit center))))
