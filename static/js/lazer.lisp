(defpackage cl-shigi-simulator/static/js/lazer
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :parenscript
        :cl-web-2d-game
        :cl-shigi-simulator/static/js/tools)
  (:export :shot-lazers
           :make-a-lazer
           :get-lazer-tag
           :get-lazer-num)
  (:import-from :cl-shigi-simulator/static/js/target
                :get-nearest-target
                :target-enable-p
                :get-target-tag
                :sort-targets-by-dist)
  (:import-from :cl-shigi-simulator/static/js/lazer-utils
                :calc-lazer-start-speed
                :assign-lazers-to-targets)
  (:import-from :ps-experiment/common-macros
                :with-slots-pair))
(in-package :cl-shigi-simulator/static/js/lazer)

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

(defun.ps+ make-lazer-start-state (&key rightp start-speed start-angle min-time)
  (make-lazer-state
   :start-process
   (lambda (lazer)
     (let ((speed (get-lazer-speed lazer)))
       (setf (vector-2d-x speed) start-speed)
       (setf (vector-2d-y speed) 0)
       (setf-vector-2d-angle speed (+ (* -1/2 PI)
                                      (* (if rightp 1 -1)
                                         start-angle)))))
   :process
   (lambda (lazer)
     (flet ((turn-lazer (target)
              (turn-lazer-to-target-first lazer target rightp nil)))
       (let* ((target (get-entity-param lazer :target))
              (target-enable (target-enable-p target))
              (dummy-target (get-entity-param lazer :dummy-target)))
         (when (and dummy-target (turn-lazer dummy-target))
           (set-entity-param lazer :dummy-target nil))
         (decf min-time)
         (when (<= min-time 0)
           (if target-enable
               (make-lazer-first-homing-state rightp)
               (unless dummy-target
                 (make-lazer-lost-state)))))))))

(defun.ps+ make-lazer-first-homing-state (rightp)
  "This only can rotate to one direction (right or left)."
  (make-lazer-state
   :process
   (lambda (lazer)
     (if (target-enable-p (get-entity-param lazer :target))
         (when (turn-lazer-to-target-first lazer (get-entity-param lazer :target) rightp)
           (make-lazer-homing-state))
         (make-lazer-lost-state)))))

(defun.ps+ make-lazer-homing-state ()
  "This can rotate to any direction (right or left)."
  (make-lazer-state
   :process
   (lambda (lazer)
     (if (target-enable-p (get-entity-param lazer :target))
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
             (get-entity-param lazer :rot-speed))
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
                          (get-entity-param lazer :rot-speed) rightp)))
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
                             (get-entity-param lazer :rot-speed)))))
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
  (check-entity-tags :lazer)
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
      (incf-vector-2d new-point speed)
      (geometry.compute-bounding-sphere)
      (setf geometry.vertices-need-update t))))

(defun.ps+ process-lazer-duration (entity)
  (check-entity-tags :lazer)
  (let ((max-duration (get-entity-param entity :max-duration)))
    (when (= max-duration 0)
      (change-lazer-state entity (make-lazer-stop-state)))
    (set-entity-param entity :max-duration (1- max-duration))))

(defun.ps+ adjust-collision-point (lazer target)
  (check-entity-tags lazer :lazer)
  (let ((lazer-pnt (calc-global-point lazer))
        (parent-pnt (calc-parent-global-point lazer)))
    (labels ((calc-mid-pnt-f (dst pnt1 pnt2)
               (setf (vector-2d-x dst) (/ (+ (vector-2d-x pnt1) (vector-2d-x pnt2)) 2))
               (setf (vector-2d-y dst) (/ (+ (vector-2d-y pnt1) (vector-2d-y pnt2)) 2))
               dst))
      (let ((dummy (make-ecs-entity))
            (head (clone-point-2d lazer-pnt))
            (next (transformf-point (clone-point-2d (get-entity-param lazer :pre-point))
                                    parent-pnt))
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
        (copy-vector-2d-to (get-ecs-component 'point-2d lazer)
                           (transformf-point-inverse next parent-pnt))))))

(defun.ps+ process-lazer-collision (mine target)
  (let ((true-target (get-entity-param mine :target)))
    (when (and true-target
               (= (ecs-entity-id target)
                  (ecs-entity-id true-target)))
      (adjust-collision-point mine target)
      (change-lazer-state mine (make-lazer-stop-state)))))

;; The start-angle is a relative angle to the vector (0, -1)
(defun.ps+ make-a-lazer (&key rightp start-point target
                              start-angle start-speed rot-speed start-offset
                              dummy-target-offset)
  (check-type start-offset vector-2d)
  (when target
    (check-entity-tags target (get-target-tag)))
  (let ((lazer (make-ecs-entity))
        (dummy-target (make-ecs-entity))
        (num-pnts (get-param :lazer :tail-length))
        (pnt-list '()))
    (add-entity-tag lazer :lazer)
    (with-slots (x y) start-point
      (dotimes (i num-pnts)
        (push (list 0 0) pnt-list))
      (let ((start-x (+ x (vector-2d-x start-offset)))
            (start-y (+ y (vector-2d-y start-offset))))
        (add-ecs-component-list
         dummy-target
         (make-point-2d :x (+ x (vector-2d-x dummy-target-offset))
                        :y (+ y (vector-2d-y dummy-target-offset))))
        (setf (ecs-entity-parent dummy-target)
              (get-default-ecs-entity-parent))
        (add-ecs-component-list
         lazer
         (make-point-2d :x start-x :y start-y)
         (make-model-2d :model (make-lines :pnt-list pnt-list :color #xff0000)
                        :depth (get-depth :lazer))
         (make-script-2d :func #'(lambda (entity)
                                   (process-lazer-state lazer)
                                   (update-lazer-points entity)
                                   (process-lazer-duration entity)))
         (make-physic-circle :r 0 :on-collision #'process-lazer-collision
                             :target-tags (list (get-target-tag)))
         (init-entity-params :duration-after-stop num-pnts
                             :duration-after-lost 30
                             :max-duration 300
                             :pre-point (make-point-2d :x start-x :y start-y)
                             :target target
                             :dummy-target dummy-target
                             :speed (make-speed-2d)
                             :lazer-state nil
                             :rot-speed rot-speed))
        ;; Note about start-angle:
        ;; A caller sets start-angle as tangential direction of expected circular orbit.
        ;; But to put a lazer into the orbit it is required to adjust the angle by
        ;; half of its rotation speed.
        (change-lazer-state lazer (make-lazer-start-state
                                   :rightp rightp
                                   :start-speed start-speed
                                   :start-angle (- start-angle (/ rot-speed 2))
                                   :min-time (get-param :lazer-state :start :time)))))
    lazer))

(defun.ps+ shot-lazers (player)
  (check-entity-tags player :player)
  (let* ((pnt (calc-global-point player))
         (parent-pnt (calc-parent-global-point player))
         (half-num (get-param :lazer-maker :half-num))
         (sorted-target-list (sort-targets-by-dist pnt))
         (assigned-target-list (assign-lazers-to-targets (* 2 half-num) sorted-target-list))
         (start-min-angle (get-param :lazer-maker :start-angle :min))
         (start-max-angle (get-param :lazer-maker :start-angle :max))
         (target-min-angle (get-param :lazer-maker :target-angle :min))
         (target-max-angle (get-param :lazer-maker :target-angle :max))
         (offset-x (get-param :lazer-maker :start-offset :x))
         (offset-y (get-param :lazer-maker :start-offset :y)))
    (assert (or (not assigned-target-list)
                (= (length assigned-target-list)
                   (* 2 half-num))))
    (dotimes (i half-num)
      (let* ((start-angle (lerp-scalar start-min-angle start-max-angle
                                       (/ i (1- half-num))))
             (target-angle (lerp-scalar target-max-angle target-min-angle
                                        (/ i (1- half-num))))
             (speed (calc-lazer-start-speed
                     :dummy-pnt
                     (make-vector-2d :x (get-param :lazer-maker :dummy-target1 :x)
                                     :y (get-param :lazer-maker :dummy-target1 :y))
                     :dummy-angle target-angle
                     :start-pnt (make-vector-2d :x offset-x :y offset-y)
                     :start-angle start-angle
                     :rot-speed (get-param :lazer :rot-speed))))
        (dolist (rightp '(nil t))
          (add-ecs-entity-to-buffer
           (make-a-lazer :start-point (transformf-point-inverse (clone-point-2d pnt) parent-pnt)
                         :target (pop assigned-target-list)
                         :rightp rightp
                         :start-speed speed
                         :start-angle start-angle
                         :rot-speed (get-param :lazer :rot-speed)
                         :start-offset (make-vector-2d :x (* offset-x (if rightp 1 -1))
                                                       :y offset-y)
                         :dummy-target-offset (make-vector-2d
                                               :x (* (get-param :lazer-maker :dummy-target1 :x) (if rightp 1 -1))
                                               :y (get-param :lazer-maker :dummy-target1 :y)))))))))

;; --- utils --- ;;

(defun.ps+ get-lazer-tag ()
  :lazer)

(defun.ps+ get-lazer-num ()
  (* 2 (get-param :lazer-maker :half-num)))
