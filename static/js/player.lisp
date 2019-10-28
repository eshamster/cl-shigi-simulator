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
     (make-model-2d :model (make-wired-regular-polygon :r r :n 100
                                                       :color (get-param :player :color))
                 :depth (get-depth :player))
     (make-point-2d :x 0 :y 0))
    ring))

(defun.ps+ make-player-body ()
  (let ((body (make-ecs-entity))
        (r (get-param :player :body-r)))
    (add-ecs-component-list
     body
     (make-model-2d :model (make-solid-regular-polygon :r r :n 100
                                                       :color (get-param :player :color))
                    :depth (get-depth :player))
     (make-point-2d :x 0 :y 0))
    body))

(defvar.ps+ *player* nil)

(defun.ps+ trigger-player-lazer ()
  (when *player*
    (set-entity-param *player* :lazer-triggered-p t)))

(defun.ps move-player (player)
  (let ((speed (get-param :player :speed))
        (r (get-param :player :body-r)))
    (with-ecs-components (point-2d) player
      (macrolet ((move (direction move)
                   `(when (key-down-p ,direction) ,move)))
        (move :left  (decf point-2d.x speed))
        (move :right (incf point-2d.x speed))
        (move :down  (decf point-2d.y speed))
        (move :up    (incf point-2d.y speed)))
      (macrolet ((fix-position (op place value)
                   `(when (,op ,place ,value)
                      (setf ,place ,value))))
        (fix-position < point-2d.x r)
        (fix-position > point-2d.x (- (get-param :play-area :width) r))
        (fix-position < point-2d.y r)
        (fix-position > point-2d.y (- (get-param :play-area :height) r))))))

(defun.ps+ control-player (player)
  (declare (ignore player))
  (when (key-down-now-p :c)
    (trigger-player-lazer)))

(defun.ps+ make-player-center ()
  (let ((body (make-ecs-entity)))
    (add-entity-tag body :player)
    (add-ecs-component-list
     body
     (make-point-2d :x (/ (get-param :play-area :width) 2) :y #y100)
     (make-script-2d :func #'(lambda (player)
                               (control-player player)
                               (move-player player)
                               (when (get-entity-param *player* :lazer-triggered-p)
                                 (set-entity-param *player* :lazer-triggered-p nil)
                                 (shot-lazers player))))
     (init-entity-params :lazer-triggered-p nil))
    body))

;; --- controller --- ;;

(defvar.ps+ *moved-by-touch-p* nil)
(defvar.ps+ *pre-touch-point* (make-vector-2d))

(defun touch-event-touches (e)
  (declare (ignore e))
  (error "touch-event-touches is not implemeted in Common Lisp"))

(defun.ps+ update-vector-by-touch (target touch-event)
  (let ((point (aref (touch-event-touches touch-event) 0)))
    (with-slots (x y) point
      (setf (vector-2d-x target) x)
      (setf (vector-2d-y target) y))))

(defun.ps+ initialize-player-controller (player)
  (add-touch-start-callback
   (lambda (e)
     (update-vector-by-touch *pre-touch-point* e)))
  (add-touch-move-callback
   (lambda (e)
     (let ((center (get-ecs-component 'point-2d player))
           (diff-point (clone-vector-2d *pre-touch-point*)))
       (setf *moved-by-touch-p* t)
       (update-vector-by-touch *pre-touch-point* e)
       (decf-vector-2d diff-point *pre-touch-point*)
       (decf-vector-2d center diff-point))))
  (add-touch-end-callback
   (lambda (e)
     (when (= (length (touch-event-touches e)) 0)
       (when *moved-by-touch-p*
         (setf *moved-by-touch-p* nil)
         (trigger-player-lazer))))))

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
    (initialize-player-controller center)
    (setf *player* center)))
