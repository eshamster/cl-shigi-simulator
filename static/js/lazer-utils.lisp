(defpackage cl-shigi-simulator/static/js/lazer-utils
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :parenscript
        :cl-web-2d-game)
  (:export :calc-lazer-start-speed
           :assign-lazers-to-targets)
  (:import-from :cl-shigi-simulator/static/js/target
                :get-target-num-lazer-to-destroy))
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

(defun.ps+ assign-lazers-to-targets (num-lazer sorted-targets)
  "Assign lazers to targets according to duration of each target.
It expects that sorted-target is sorted by its distance from lazer launcher."
  (assert (> num-lazer 0))
  (unless sorted-targets
    (return-from assign-lazers-to-targets nil))
  (let ((result (list))
        (rest-lazer num-lazer))
    ;; Assign lazers to each target until its duration is out or lazer is shourt
    (dolist (target sorted-targets)
      (when (<= rest-lazer 0)
        (return))
      (let* ((required (get-target-num-lazer-to-destroy target))
             (assigned (min required rest-lazer)))
        (dotimes (i assigned)
          (push target result))
        (decf rest-lazer assigned)))
    (assert (>= rest-lazer 0))
    ;; Assign rest lazers by round-robin
    (labels ((rec (rest-targets)
               (when (<= rest-lazer 0)
                 (return-from rec))
               (let ((target (car rest-targets))
                     (rest (cdr rest-targets)))
                 (push target result)
                 (decf rest-lazer)
                 (rec (if (> (length rest) 0)
                          rest
                          sorted-targets)))))
      (rec sorted-targets))
    (assert (= (length result) num-lazer))
    (reverse result)))
