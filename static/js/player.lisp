(defpackage cl-shigi-simulator/static/js/player
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :parenscript
        :cl-web-2d-game
        :cl-shigi-simulator/static/js/lazer
        :cl-shigi-simulator/static/js/shigi
        :cl-shigi-simulator/static/js/tools)
  (:export :make-player
           :check-player)
  (:import-from :cl-shigi-simulator/static/js/target
                :get-nearest-target)
  (:import-from :ps-experiment/common-macros
                :with-slots-pair))
(in-package :cl-shigi-simulator/static/js/player)

(enable-ps-experiment-syntax)

;; --- body --- ;;

(defun.ps+ make-player-ring ()
  (let ((ring (make-ecs-entity))
        (r (get-param :player :ring-r)))
    (add-ecs-component-list
     ring
     (make-model-2d :model (make-wired-circle :r r
                                              :color (get-param :player :color))
                    :depth (get-depth :player))
     (make-point-2d :x 0 :y 0))
    ring))

(defun.ps+ make-player-body ()
  (let ((body (make-ecs-entity))
        (r (get-param :player :body-r)))
    (add-ecs-component-list
     body
     (make-model-2d :model (make-solid-circle :r r
                                              :color (get-param :player :color))
                    :depth (get-depth :player))
     (make-point-2d :x 0 :y 0))
    body))

(defun.ps+ trigger-player-lazer (player)
  (when player
    (set-entity-param player :lazer-triggered-p t)))

(defun.ps+ move-player-at (player global-x global-y)
  (let ((r (get-param :player :body-r))
        (local-pnt (transformf-point-inverse
                    (make-point-2d :x global-x :y global-y)
                    (calc-parent-global-point player))))
    (with-ecs-components (point-2d) player
      (with-slots (x y) point-2d
        (setf x (point-2d-x local-pnt)
              y (point-2d-y local-pnt))
        (macrolet ((fix-position (op place value)
                     `(when (,op ,place ,value)
                        (setf ,place ,value))))
          (fix-position < x r)
          (fix-position > x (- (get-param :play-area :width) r))
          (fix-position < y r)
          (fix-position > y (- (get-param :play-area :height) r)))))))

(defun.ps+ move-player-by (player diff-x diff-y)
  (let ((pnt (calc-global-point player)))
    (move-player-at player
                    (+ (point-2d-x pnt) diff-x)
                    (+ (point-2d-y pnt) diff-y))))

(defun.ps+ move-player-to (player dir)
  (let ((speed (get-param :player :speed)))
    (ecase dir
      (:right (move-player-by player speed 0))
      (:left  (move-player-by player (* -1 speed) 0))
      (:up    (move-player-by player 0 speed))
      (:down  (move-player-by player 0 (* -1 speed))))))

(defun.ps+ make-player-center ()
  (let ((body (make-ecs-entity)))
    (add-entity-tag body :player)
    (add-ecs-component-list
     body
     (make-point-2d :x (/ (get-param :play-area :width) 2) :y #y100)
     (make-script-2d :func #'(lambda (player)
                               (when (get-entity-param player :lazer-triggered-p)
                                 (set-entity-param player :lazer-triggered-p nil)
                                 (shot-lazers player))))
     (init-entity-params :lazer-triggered-p nil))
    body))

;; --- controller --- ;;

(defun.ps+ add-controler (player)
  (add-keyboard-controler player)
  (add-touch-controler player))

;; - keybord - ;;

(defun.ps+ add-keyboard-controler (player)
  (let ((ctrl (make-ecs-entity)))
    (add-ecs-component-list
     ctrl
     (make-script-2d :func (lambda (entity)
                             (declare (ignore entity))
                             (control-player-by-keyboard player))))
    (add-ecs-entity ctrl)))

(defun.ps+ control-player-by-keyboard (player)
  (flet ((move (dir)
           (when (key-down-p dir)
             (move-player-to player dir))))
    (move :right)
    (move :left)
    (move :up)
    (move :down))
  (when (key-down-now-p :c)
    (trigger-player-lazer player)))

;; - touch - ;;

(defun.ps+ add-touch-controler (player)
  (let ((ctrl (make-ecs-entity)))
    (add-ecs-component-list
     ctrl
     (make-script-2d :func (lambda (entity)
                             (control-player-by-touch entity player)))
     (init-entity-params :pre-pnt (make-point-2d)))
    (add-ecs-entity ctrl)))

(defun.ps+ control-player-by-touch (ctrl player)
  (let ((state (get-total-touch-state)))
    (when (or (eq state :down)
              (eq state :down-now))
      (let ((pre-pnt (get-entity-param ctrl :pre-pnt))
            (x (get-total-touch-x))
            (y (get-total-touch-y)))
        (when (eq state :down)
          (move-player-by player
                          (- x (point-2d-x pre-pnt))
                          (- y (point-2d-y pre-pnt))))
        (setf (point-2d-x pre-pnt) x
              (point-2d-y pre-pnt) y)))
    (when (eq state :up-now)
      (trigger-player-lazer player))))

;; --- tools --- ;;

(defun.ps+ check-player (player)
  (check-entity-tags player :player))

;; --- make --- ;;

(defun.ps+ make-player ()
  (let ((center (make-player-center))
        (body (make-player-body))
        (ring (make-player-ring)))
    (add-ecs-entity center)
    (add-ecs-entity body center)
    (add-ecs-entity ring center)
    (add-controler center)
    center))
