(in-package :cl-user)
(defpackage cl-shigi-simulator.static.js.input
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :parenscript
        :cl-shigi-simulator.static.js.tools))
(in-package :cl-shigi-simulator.static.js.input)

(enable-ps-experiment-syntax)

(defun.ps process-input ()
  (process-keyboard-input)
  (process-mouse-input))

;; --- common --- ;;

(defun.ps+ calc-next-input-state (now-state device-state)
  "now-state = :down-now | :down | :up-now | :up
device-state = boolean-value"
  (if device-state
      (case now-state
        ((:down-now :down) :down)
        (t :down-now))
      (case now-state
        ((:up-now :up) :up)
        (t :up-now))))

;; ---- keyboard ---- ;;

(defvar.ps keyboard (new (#j.THREEx.KeyboardState#)))
(defvar.ps key-status (make-hash-table))

(defun.ps is-key-down (keyname)
  "Return if the key is down"
  (let ((value (gethash (keyboard.keyname-to-keycode keyname) key-status)))
    (and (not (null value))
         (or (eq value :down) (eq value :down-now)))))

(defun.ps is-key-down-now (keyname)
  "Return if the key is down just in this frame"
  (let ((value (gethash (keyboard.keyname-to-keycode keyname) key-status)))
    (and (not (null value))
         (eq value :down-now))))

(defun.ps is-key-up-now (keyname)
  "Return if the key is down just in this frame"
  (let ((value (gethash (keyboard.keyname-to-keycode keyname) key-status)))
    (and (not (null value))
         (eq value :up-now))))

(defun.ps process-keyboard-input ()
  (maphash (lambda (k v)
             (setf (gethash k key-status)
                   (calc-next-input-state (gethash k key-status) v)))
           keyboard.key-codes))

;; ---- mouse ---- ;;

;; variables

(defvar.ps _mouse-x -100)
(defvar.ps _mouse-y -100)
(defvar.ps _mouse-left :up)

(defvar.ps *mouse-x-buffer* -100)
(defvar.ps *mouse-y-buffer* -100)
(defvar.ps *mouse-left-buffer* nil)

(defvar.ps *mouse-left-button-id* 1)
(defvar.ps *mouse-right-button-id* 3)

;; main

(defun.ps process-mouse-input ()
  (setf _mouse-x *mouse-x-buffer*)
  (setf _mouse-y *mouse-y-buffer*)
  (setf _mouse-left
        (calc-next-input-state _mouse-left
                               *mouse-left-buffer*)))

;; interfaces

(defun.ps get-mouse-x () _mouse-x)
(defun.ps get-mouse-y () _mouse-y)
(defun.ps get-left-mouse-state () _mouse-left)

;; (private)
(defun.ps set-mouse-point (x y)
  (let* ((renderer (document.query-selector "#renderer"))
         (canvas (renderer.query-selector "canvas")))
    (setf *mouse-x-buffer* (- x renderer.offset-left
                              (get-param :play-area :x)))
    (setf *mouse-y-buffer* (- canvas.height
                              (- y renderer.offset-top)
                              (get-param :play-area :y)))))

;; callbacks

(defun.ps on-mouse-move-event (e)
  (set-mouse-point e.client-x e.client-y))

(defun.ps on-mouse-down-event (e)
  (when (= e.which *mouse-left-button-id*)
    (setf *mouse-left-buffer* t)))

(defun.ps on-mouse-up-event (e)
  (when (= e.which *mouse-left-button-id*)
    (setf *mouse-left-buffer* nil)))

(defun.ps set-point-by-touch (e)
  (let ((point (aref e.touches 0)))
    (set-mouse-point point.client-x point.client-y)))

(defvar.ps+ *moved-by-touch-p* nil)

(defun.ps on-touch-start (e)
  (set-point-by-touch e)
  (setf *mouse-left-buffer* t))

(defun.ps on-touch-end (e)
  (when (= e.touches.length 0)
    (setf *mouse-left-buffer* nil)
    (when *moved-by-touch-p*
      (setf *moved-by-touch-p* nil)
      (trigger-player-lazer))))

(defun.ps on-touch-move-event (e)
  (set-point-by-touch e)
  (let ((point (aref e.touches 0))) 
    ;; test
    (let* ((player (find-a-entity-by-tag "player"))
           (center (get-ecs-component 'point-2d player)))
      (setf *moved-by-touch-p* t)
      (setf center.x *mouse-x-buffer*)
      (setf center.y *mouse-y-buffer*))))
