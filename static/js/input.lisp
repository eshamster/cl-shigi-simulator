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

(defun.ps process-input ()

  (maphash (lambda (k v)
             (symbol-macrolet ((value (gethash k key-status)))
               (if v
                   (case value
                     (:down-now (setf value :down))
                     (:down)
                     (t (setf value :down-now)))
                   (case value
                     (:up-now (setf value :up))
                     (:up)
                     (t (setf value :up-now))))))
           keyboard.key-codes))

;; ---- mouse ---- ;;

(defvar.ps _mouse-x -100)
(defvar.ps _mouse-y -100)

(defun.ps on-mouse-move-event (e)
  (let* ((renderer (document.query-selector "#renderer"))
         (canvas (renderer.query-selector "canvas")))
    (setf _mouse-x (- e.client-x renderer.offset-left))
    (setf _mouse-y (- canvas.height
                      (- e.client-y renderer.offset-top)))))

(defun.ps on-mouse-down-event (e)
  (do-tagged-ecs-entities (part "shigi-part")
    (when (> (random) 0.5)
      (toggle-shigi-part part))))

(defun.ps on-touch-start (e)
  (when (= e.touches.length 2)
    (do-tagged-ecs-entities (part "shigi-bit")
      (when (> (random) 0.5)
        (toggle-shigi-part part)))))

(defun.ps on-touch-move-event (e)
  (let* ((renderer (document.query-selector "#renderer"))
         (canvas (renderer.query-selector "canvas")))
    (let ((point (aref e.touches 0)))
      (setf _mouse-x (- point.client-x renderer.offset-left))
      (setf _mouse-y (- canvas.height
                        (- point.client-y renderer.offset-top)))
      ;; test
      (let* ((player (find-a-entity-by-tag "player"))
             (center (get-ecs-component 'point-2d player)))
        (setf center.x _mouse-x)
        (setf center.y _mouse-y)))))

(defun.ps get-mouse-x () _mouse-x)
(defun.ps get-mouse-y () _mouse-y)
