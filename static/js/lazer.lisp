(in-package :cl-user)
(defpackage cl-shigi-simulator.static.js.lazer
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :parenscript
        :cl-web-2d-game
        :cl-shigi-simulator.static.js.shigi
        :cl-shigi-simulator.static.js.tools)
  (:export :shot-lazers)
  (:import-from :ps-experiment/common-macros
                :with-slots-pair))
(in-package :cl-shigi-simulator.static.js.lazer)

(enable-ps-experiment-syntax)

(defvar.ps+ *enable-lazer-debug* nil)

;; --- lazer state --- ;;

(defstruct.ps+ lazer-state
    (start-process (lambda (lazer) (declare (ignore lazer))))
  (process (lambda (lazer) (declare (ignore lazer))))
  (end-process (lambda (lazer) (declare (ignore lazer)))))

(defun.ps+ change-lazer-state (lazer next-state
                                     &optional (state (get-entity-param lazer :lazer-state)))
  (check-type next-state lazer-state)
  (when state
    (funcall (lazer-state-end-process state) lazer))
  (funcall (lazer-state-start-process next-state) lazer)
  (set-entity-param lazer :lazer-state next-state))

(defun.ps+ process-lazer-state (lazer)
  (let* ((state (get-entity-param lazer :lazer-state))
         (next-state (funcall (lazer-state-process state) lazer)))
    (when next-state
      (change-lazer-state lazer next-state state))))

(defun.ps+ make-lazer-start-state (&key rightp first-speed first-angle min-time)
  (make-lazer-state
   :start-process
   (lambda (lazer)
     (let ((speed (get-lazer-speed lazer)))
       (setf (vector-2d-x speed) first-speed)
       (setf (vector-2d-y speed) 0)
       (setf-vector-2d-angle speed (+ (* -1/2 PI)
                                      (* (if rightp 1 -1)
                                         first-angle)))))
   :process
   (lambda (lazer)
     (flet ((turn-lazer (target)
              (turn-lazer-to-target-first lazer target rightp nil)))
       (let* ((target (get-entity-param lazer :target))
              (target-valid-p (shigi-part-valid-p target))
              (dummy-target (get-entity-param lazer :dummy-target)))
         (when (and dummy-target (turn-lazer dummy-target))
           (set-entity-param lazer :dummy-target nil))
         (decf min-time)
         (when (<= min-time 0)
           (if target-valid-p
               (make-lazer-first-homing-state rightp)
               (unless dummy-target
                 (make-lazer-lost-state)))))))))

(defun.ps+ make-lazer-first-homing-state (rightp)
  "This only can rotate to one direction (right or left)."
  (make-lazer-state
   :process
   (lambda (lazer)
     (if (shigi-part-valid-p (get-entity-param lazer :target))
         (when (turn-lazer-to-target-first lazer (get-entity-param lazer :target) rightp)
           (make-lazer-homing-state))
         (make-lazer-lost-state)))))

(defun.ps+ make-lazer-homing-state ()
  "This can rotate to any direction (right or left)."
  (make-lazer-state
   :process
   (lambda (lazer)
     (if (shigi-part-valid-p (get-entity-param lazer :target))
         (turn-lazer-to-target lazer)
         (make-lazer-lost-state)))))

(defun.ps+ make-lazer-lost-state ()
  (make-lazer-state
   :start-process
   (lambda (lazer)
     (delete-ecs-component-type 'physic-2d lazer))
   :process
   (lambda (lazer)
     (accell-lazer-speed lazer (get-param :lazer :accell))
     (let ((duration (get-entity-param lazer :duration-after-lost)))
       (set-entity-param lazer :duration-after-lost (1- duration))
       (when (<= duration 0)
         (make-lazer-stop-state))))))

(defun.ps+ make-lazer-stop-state ()
  (make-lazer-state
   :start-process
   (lambda (lazer)
     (register-next-frame-func
      (lambda () (delete-ecs-component-type 'physic-2d lazer)))
     (let ((speed-2d (get-lazer-speed lazer)))
       (setf (vector-2d-x speed-2d) 0
             (vector-2d-y speed-2d) 0)))
   :process
   (lambda (lazer)
     (let ((duration (get-entity-param lazer :duration-after-stop)))
       (when (< duration 0)
         (delete-ecs-entity lazer))
       (set-entity-param lazer :duration-after-stop (1- duration))
       nil))))

;; --- --- ;;

(defun.ps+ get-lazer-speed (lazer)
  (get-entity-param lazer :speed))

(defun.ps+ accell-lazer-speed (lazer accell)
  (let ((speed-2d (get-lazer-speed lazer)))
    (setf-vector-2d-abs
     speed-2d
     (max (min (+ (vector-2d-abs speed-2d) accell)
               (get-param :lazer :max-speed))
          (get-param :lazer :min-speed)))))

(defun.ps+ calc-angle-to-target (lazer target)
  (let ((lazer-pnt (calc-global-point lazer))
        (target-pnt (calc-global-point target)))
    (vector-2d-angle (decf-vector-2d (clone-vector-2d target-pnt) lazer-pnt))))

(defun.ps+ adjust-angle-in-range (angle)
  (cond ((> angle PI)
         (adjust-angle-in-range (- angle (* 2 PI))))
        ((< angle (* -1 PI))
         (adjust-angle-in-range (+ angle (* 2 PI))))
        (t angle)))

(defun.ps+ adjust-lazer-speed (lazer diff-angle)
  (accell-lazer-speed
   lazer
   (* (get-param :lazer :accell)
      (if (< (abs (adjust-angle-in-range diff-angle))
             (get-param :lazer :rot-speed))
          1 -1))))

(defun.ps+ adjust-to-target-angle (now target rot-speed)
  (let ((diff (- target now)))
    (when (> diff PI)
      (decf target (* 2 PI)))
    (when (< diff (* -1 PI))
      (incf target (* 2 PI))))
  (adjust-to-target now target rot-speed))

(defun.ps+ adjust-to-target-angle-first (now target rot-speed rightp)
  (labels ((adjust-target-angle (angle)
             (if rightp
                 (if (> now angle)
                     (adjust-target-angle (+ angle (* 2 PI)))
                     angle)
                 (if (< now angle)
                     (adjust-target-angle (- angle (* 2 PI)))
                     angle))))
    (adjust-to-target now (adjust-target-angle target) rot-speed)))

(defun.ps+ turn-lazer-to-target-first (lazer target rightp &optional (change-speed-p t))
  "Note: return t if the angle become same to the target angle"
  (unless (null target)
    (let* ((speed-2d (get-lazer-speed lazer))
           (now-angle (vector-2d-angle speed-2d))
           (target-angle (calc-angle-to-target lazer target))
           (new-angle (adjust-to-target-angle-first
                          now-angle target-angle
                          (get-param :lazer :rot-speed) rightp)))
      (when change-speed-p
        (adjust-lazer-speed lazer (- target-angle now-angle)))
      (setf-vector-2d-angle speed-2d new-angle)
      (= target-angle new-angle))))

(defun.ps+ turn-lazer-to-target (lazer)
  (unless (null (get-entity-param lazer :target))
    (let* ((speed-2d (get-lazer-speed lazer))
           (target (get-entity-param lazer :target))
           (now-angle (vector-2d-angle speed-2d))
           (target-angle (calc-angle-to-target lazer target)))
      (adjust-lazer-speed lazer (- target-angle now-angle))
      (setf-vector-2d-angle speed-2d
                            (adjust-to-target-angle
                             now-angle target-angle
                             (get-param :lazer :rot-speed)))))
  nil)

(defun.ps get-lazer-geometry (lazer)
  (with-ecs-components (model-2d) lazer
    model-2d.model.geometry))

(defun.ps decf-offset-from-lazer-tails (geometry offset-x offset-y)
  (let* ((pnt-list geometry.vertices)
         (len (length pnt-list)))
    (dotimes (i (1- len))
      (let ((index (- len (1+ i))))
        (with-slots-pair ((x y) (aref pnt-list index))
          (decf x offset-x)
          (decf y offset-y))))
    (geometry.compute-bounding-sphere)
    (setf geometry.vertices-need-update t)))

(defun.ps update-lazer-points (lazer)
  ;; Note that a model space is relative to the point of the entity. (Ex. (0, 0) in the model space is the same to the point of the entity in the absolute coordinate.)
  (check-entity-tags "lazer")
  (with-ecs-components ((new-point point-2d)) lazer
    (let* ((speed (get-entity-param lazer :speed))
           (geometry (get-lazer-geometry lazer))
           (pnt-list geometry.vertices)
           (pre-point (get-entity-param lazer :pre-point))
           (len (length pnt-list)))
      (dotimes (i (1- len))
        (let ((index (- len (1+ i))))
          (with-slots-pair (((x1 x) (y1 y)) (aref pnt-list index)
                            ((x0 x) (y0 y)) (aref pnt-list (1- index)))
            (setf x1 x0 y1 y0))))
      (decf-offset-from-lazer-tails geometry (vector-2d-x speed) (vector-2d-y speed))
      (copy-point-2d-to pre-point new-point)
      (incf-2d-vector new-point speed)
      (geometry.compute-bounding-sphere)
      (setf geometry.vertices-need-update t))))

(defun.ps+ process-lazer-duration (entity)
  (check-entity-tags "lazer")
  (let ((max-duration (get-entity-param entity :max-duration)))
    (when (= max-duration 0)
      (change-lazer-state entity (make-lazer-stop-state)))
    (set-entity-param entity :max-duration (1- max-duration))))

(defun.ps adjust-collision-point (lazer target)
  (check-entity-tags lazer "lazer")
  (with-ecs-components ((lazer-pnt point-2d)) lazer
    (labels ((calc-mid-pnt-f (dst pnt1 pnt2)
               (setf (vector-2d-x dst) (/ (+ (vector-2d-x pnt1) (vector-2d-x pnt2)) 2))
               (setf (vector-2d-y dst) (/ (+ (vector-2d-y pnt1) (vector-2d-y pnt2)) 2))
               dst))
      (let ((dummy (make-ecs-entity))
            (head (clone-point-2d lazer-pnt))
            (next (clone-vector-2d (get-entity-param lazer :pre-point)))
            (mid (make-point-2d)))
        (calc-mid-pnt-f mid head next)
        (add-ecs-component-list
         dummy
         mid
         (make-physic-circle :r 0))
        (dotimes (i 6)
          (if (collide-entities-p dummy target)
              (copy-vector-2d-to head mid)
              (copy-vector-2d-to next mid))
          (calc-mid-pnt-f mid head next))
        (decf-offset-from-lazer-tails (get-lazer-geometry lazer)
                                      (- (vector-2d-x head) (vector-2d-x lazer-pnt))
                                      (- (vector-2d-y head) (vector-2d-y lazer-pnt)))
        (copy-vector-2d-to lazer-pnt head)))))

(defun.ps process-lazer-collision (mine target)
  (let ((true-target (get-entity-param mine :target)))
    (when (and true-target
               (= (ecs-entity-id target)
                  (ecs-entity-id true-target)))
     (adjust-collision-point mine target)
     (change-lazer-state mine (make-lazer-stop-state)))))

(defun.ps+ check-first-lazer-angle (first-angle
                                    dummy-pnt
                                    start-pnt
                                    radious
                                    formed-angle-of-tangential-line)
  "Check if the calculated first lazer angle meets some conditions"
  ;; In the following note, a circle that is a trajectory of a lazer started from "start-pnt" and
  ;; whose radious is "radious" is imagined.
  ;; The "pnt" is the intersection point of two tangential lines. (A tangential line touches to
  ;; the circle at the "start-pnt", and another one should pass through the "dummy-pnt".) Then,
  ;; check if the "pnt" nears to start-pnt than dummy-pnt or not. If so, the lazer can pass
  ;; dummy-pnt by the angle "dummy-angle" (this is an argument of calc-first-lazer-angle).
  ;; Otherwize, it can't because the line from "pnt" to "dummy-pnt" is not a tangential line.
  (let* ((len-to-pnt (/ radious
                        (tan (/ formed-angle-of-tangential-line 2))))
         (pnt (make-vector-2d
               :x (+ (vector-2d-x start-pnt)
                     (* len-to-pnt (cos first-angle)))
               :y (+ (vector-2d-y start-pnt)
                     (* len-to-pnt (sin first-angle))))))
    (when *enable-lazer-debug*
      (let ((player (find-a-entity-by-tag "player")))
        (draw-debug-point-by-time :point pnt
                                  :parent player)
        (draw-debug-line-by-time :point1 pnt
                                 :point2 dummy-pnt
                                 :parent player)
        (draw-debug-line-by-time :point1 pnt
                                 :point2 start-pnt
                                 :parent player)))
    (add-to-event-log (calc-dist-p2 pnt start-pnt))
    (add-to-event-log (calc-dist-p2 pnt dummy-pnt))
    (<= (calc-dist-p2 pnt start-pnt)
        (calc-dist-p2 pnt dummy-pnt))))

(defun.ps+ calc-first-lazer-angle (dummy-pnt dummy-angle start-pnt
                                   speed rot-speed)
  "Caluclate first angle of a lazer. When the lazer starts from 'start-pnt' by the angle with
'speed' and 'rot-speed' (axial speed), it passes throught 'dummy-pnt' at angle 'dummy-angle'."
  (let* ((pnt-on-line (make-vector-2d
                       :x (+ (vector-2d-x dummy-pnt)
                             (cos dummy-angle))
                       :y (+ (vector-2d-y dummy-pnt)
                             (sin dummy-angle))))
         (dist-to-line (calc-dist-to-line start-pnt
                                          dummy-pnt pnt-on-line))
         (radious (/ speed rot-speed)))
    (if (< (abs (- dist-to-line radious)) radious)
        (let* ((temp (acos (/ (- dist-to-line radious) radious)))
               (first-angle (* -1 (- PI temp dummy-angle))))
          ;; The return value is an angle with reference to -PI/2
          (if (check-first-lazer-angle
               first-angle dummy-pnt start-pnt radious temp)
              (- first-angle (* -1 (/ PI 2)))
              PI ; error case
              ))
        PI ; error case
        )))

;; The base angle of first-angle is the angle of (0, -1)
(defun.ps make-a-lazer (&key rightp player target
                             first-angle first-speed first-offset
                             dummy-target-offset)
  (check-entity-tags player "player")
  (check-type first-offset vector-2d)
  (when target
    (check-entity-tags target "shigi-part"))
  (let ((lazer (make-ecs-entity))
        (dummy-target (make-ecs-entity))
        (num-pnts (get-param :lazer :tail-length))
        (pnt-list '()))
    (add-entity-tag lazer "lazer")
    (with-ecs-components (point-2d) player
      (with-slots (x y) point-2d
        (dotimes (i num-pnts)
          (push (list 0 0) pnt-list))
        (let ((first-x (+ x (vector-2d-x first-offset)))
              (first-y (+ y (vector-2d-y first-offset))))
          (add-ecs-component-list
           dummy-target
           (make-point-2d :x (+ x (vector-2d-x dummy-target-offset))
                          :y (+ y (vector-2d-y dummy-target-offset))))
          (add-ecs-component-list
           lazer
           (make-point-2d :x first-x :y first-y)
           (make-model-2d :model (make-lines :pnt-list pnt-list :color 0xff0000)
                          :depth (get-param :lazer :depth))
           (make-script-2d :func #'(lambda (entity)
                                     (process-lazer-state lazer)
                                     (update-lazer-points entity)
                                     (process-lazer-duration entity)))
           (make-physic-circle :r 0 :on-collision #'process-lazer-collision
                               :target-tags '("shigi-part"))
           (init-entity-params :duration-after-stop num-pnts
                               :duration-after-lost 30
                               :max-duration 300
                               :pre-point (make-vector-2d :x first-x :y first-y)
                               :target target
                               :dummy-target dummy-target
                               :speed (make-speed-2d)
                               :lazer-state nil))
          (change-lazer-state lazer (make-lazer-start-state
                                     :rightp rightp
                                     :first-speed first-speed
                                     :first-angle first-angle
                                     :min-time (get-param :lazer-state :start :time))))))
    lazer))

(defun.ps shot-lazers (player)
  (check-entity-tags player "player")
  (set-entity-param player :lazer-triggered-p nil)
  (let* ((pnt (calc-global-point player))
         (target (get-nearest-shigi-part pnt))
         (min-speed (get-param :lazer-maker :min-speed))
         (max-speed (get-param :lazer-maker :max-speed))
         (min-angle (get-param :lazer-maker :min-angle))
         (max-angle (get-param :lazer-maker :max-angle))
         (half-num (get-param :lazer-maker :half-num))
         (offset-x (get-param :lazer-maker :first-offset :x))
         (offset-y (get-param :lazer-maker :first-offset :y)))
    (dotimes (i half-num)
      (let* ((speed (lerp-scalar min-speed max-speed
                                (/ i (1- half-num))))
             (angle (calc-first-lazer-angle
                     (make-vector-2d :x (get-param :lazer-maker :dummy-target1 :x)
                                     :y (get-param :lazer-maker :dummy-target1 :y))
                     (lerp-scalar max-angle min-angle
                                  (/ i (1- half-num)))
                     (make-vector-2d :x offset-x :y offset-y)
                     speed
                     (get-param :lazer :rot-speed))))
        (dolist (rightp '(t nil))
          (add-ecs-entity-to-buffer
           (make-a-lazer :player player
                         :target target
                         :rightp rightp
                         :first-speed speed
                         :first-angle angle
                         :first-offset (make-vector-2d :x (* offset-x (if rightp 1 -1))
                                                       :y offset-y)
                         :dummy-target-offset (make-vector-2d
                                               :x (* (get-param :lazer-maker :dummy-target1 :x) (if rightp 1 -1))
                                               :y (get-param :lazer-maker :dummy-target1 :y)))))))))
