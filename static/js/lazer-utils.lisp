(defpackage cl-shigi-simulator/static/js/lazer-utils
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :parenscript
        :cl-web-2d-game)
  (:export :calc-lazer-start-speed))
(in-package :cl-shigi-simulator/static/js/lazer-utils)

(enable-ps-experiment-syntax)

(defun.ps+ calc-lazer-start-speed (&key dummy-pnt dummy-angle
                                        start-pnt start-angle
                                        rot-speed)
  "Caluclate first speed of a lazer.
 When the lazer starts from 'start-pnt' by the speed with 'start-angle'
 (it is relative value from -PI/2) and 'rot-speed' (axial speed),
it passes throught 'dummy-pnt' at angle 'dummy-angle'."
  (let* ((pnt-on-line (make-vector-2d
                       :x (+ (vector-2d-x dummy-pnt)
                             (cos dummy-angle))
                       :y (+ (vector-2d-y dummy-pnt)
                             (sin dummy-angle))))
         (dist-to-line (calc-dist-to-line
                        start-pnt dummy-pnt pnt-on-line))
         (temp (cos (- (+ (* PI 1/2) start-angle)
                       dummy-angle)))
         (radious (/ dist-to-line (1+ temp))))
    (* radious rot-speed)))
