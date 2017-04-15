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
  (:import-from :ps-experiment.common-macros
                :with-slots-pair))
(in-package :cl-shigi-simulator.static.js.lazer)

(enable-ps-experiment-syntax)

;; --- lazer state --- ;;

(defstruct.ps+ lazer-state
    (start-process (lambda (lazer) (declare (ignore lazer))))
  (process (lambda (lazer) (declare (ignore lazer))))
  (end-process (lambda (lazer) (declare (ignore lazer)))))

(defun.ps+ change-lazer-state (lazer next-state
                                     &optional (state (get-entity-param lazer :lazer-state)))
  (funcall (lazer-state-end-process state) lazer)
  (funcall (lazer-state-start-process next-state) lazer)
  (set-entity-param lazer :lazer-state next-state))

(defun.ps+ process-lazer-state (lazer)
  (let* ((state (get-entity-param lazer :lazer-state))
         (next-state (funcall (lazer-state-process state) lazer)))
    (when next-state
      (check-type next-state lazer-state)
      (change-lazer-state lazer next-state state))))

(defun.ps+ make-lazer-start-state (&key left-p first-speed rot-speed min-time)
  (make-lazer-state
   :start-process
   (lambda (lazer)
     (let ((speed (get-lazer-speed lazer)))
       (setf (vector-2d-x speed) first-speed)
       (setf (vector-2d-y speed) 0)
       (setf-vector-angle speed (* -1 PI))))
   :process
   (lambda (lazer)
     (let ((speed (get-lazer-speed lazer)))
       (setf-vector-angle speed (+ (vector-angle speed)
                                   (* rot-speed
                                      (if left-p -1 1)))))
     (decf min-time)
     (when (<= min-time 0)
       (if (shigi-part-valid-p (get-entity-param lazer :target))
           (make-lazer-first-homing-state left-p)
           (make-lazer-to-dummy-state))))))

(defun.ps+ make-lazer-first-homing-state (left-p)
  (make-lazer-state
   :process
   (lambda (lazer)
     (if (shigi-part-valid-p (get-entity-param lazer :target))
         (turn-lazer-to-target lazer)
         (make-lazer-lost-state)))))

(defun.ps+ make-lazer-to-dummy-state ()
  ; [WIP]
  (make-lazer-lost-state))

(defun.ps+ make-lazer-lost-state ()
  (make-lazer-state
   :start-process
   (lambda (lazer)
     (delete-ecs-component-type 'physic-2d lazer))))

;; --- --- ;;

(defun.ps+ get-lazer-speed (lazer)
  (get-entity-param lazer :speed))

(defun.ps+ angle-to-target (lazer target)
  (let ((lazer-pnt (calc-global-point lazer))
        (target-pnt (calc-global-point target)))
    (vector-angle (decf-vector (clone-vector-2d target-pnt) lazer-pnt))))

(defun.ps+ adjust-to-target-angle (now target speed)
  (let ((diff (- target now)))
    (when (> diff PI)
      (decf target (* 2 PI)))
    (when (< diff (* -1 PI))
      (incf target (* 2 PI))))
  (adjust-to-target now target speed))

(defun.ps+ turn-lazer-to-target (lazer)
  (unless (or (get-entity-param lazer :stop-homing-p)
              (null (get-entity-param lazer :target)))
    (let* ((speed-2d (get-lazer-speed lazer))
           (target (get-entity-param lazer :target))
           (now-angle (vector-angle speed-2d))
           (target-angle (angle-to-target lazer target)))
      (setf-vector-angle speed-2d
                         (adjust-to-target-angle
                          now-angle target-angle
                          (get-param :lazer :max-rot-speed)))))
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
      (incf-vector new-point speed)
      (geometry.compute-bounding-sphere)
      (setf geometry.vertices-need-update t))))

(defun.ps process-lazer-duration (entity)
  (check-entity-tags "lazer")
  (cond ((not (get-entity-param entity :hitp))
         (labels ((process-duration (kind)
                    (let ((max-duration (get-entity-param entity kind)))
                      (when (= max-duration 0)
                        (set-entity-param entity :hitp t))
                      (set-entity-param entity kind (1- max-duration)))))
           (process-duration :max-duration)
           (when (get-entity-param entity :stop-homing-p)
             (process-duration :duration-after-stop))))
        (t (let ((duration (get-entity-param entity :duration)))
             (when (< duration 0)
               (delete-ecs-entity entity))
             (set-entity-param entity :duration (1- duration))))))

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
               (not (get-entity-param mine :stop-homing-p))
               (= (ecs-entity-id target)
                  (ecs-entity-id true-target)))
      (let ((speed-2d (get-lazer-speed mine)))
        (setf speed-2d.x 0
              speed-2d.y 0))
      (adjust-collision-point mine target)
      (set-entity-param mine :hitp t))))

;; The base angle of first-angle is the angle of (0, -1)
(defun.ps make-lazer (&key left-p player target first-angle first-offset)
  (check-entity-tags player "player")
  (check-type first-offset vector-2d)
  (when target
    (check-entity-tags target "shigi-part"))
  (let ((lazer (make-ecs-entity))
        (num-pnts (get-param :lazer :tail-length))
        (first-speed (make-speed-2d :x (get-param :lazer :max-speed)))
        (pnt-list '()))
    (add-entity-tag lazer "lazer")
    (setf-vector-angle first-speed (- first-angle (/ PI 2)))
    (with-ecs-components (point-2d) player
      (with-slots (x y) point-2d
        (dotimes (i num-pnts)
          (push (list 0 0) pnt-list))
        (let ((first-x (+ x (vector-2d-x first-offset)))
              (first-y (+ y (vector-2d-y first-offset))))
          (add-ecs-component-list
           lazer
           (make-point-2d :x first-x :y first-y)
           (make-model-2d :model (make-lines :pnt-list pnt-list :color 0xff0000)
                          :depth (get-param :lazer :depth))
           (make-script-2d :func #'(lambda (entity)
                                     (let ((target (get-entity-param entity :target)))
                                       (unless (shigi-part-valid-p target)
                                         (set-entity-param entity :stop-homing-p t)))
                                     (process-lazer-state lazer)
                                     (update-lazer-points entity)
                                     (process-lazer-duration entity)))
           (make-physic-circle :r 0 :on-collision #'process-lazer-collision
                               :target-tags '("shigi-part"))
           (init-entity-params :duration num-pnts
                               :hitp nil
                               :stop-homing-p nil
                               :duration-after-stop 30
                               :max-duration 600
                               :pre-point (make-vector-2d :x first-x :y first-y)
                               :speed first-speed
                               :target target
                               :lazer-state (make-lazer-start-state
                                             :left-p left-p
                                             :first-speed (vector-abs first-speed)
                                             :rot-speed (/ PI 32)
                                             :min-time 8))))))
    lazer))
