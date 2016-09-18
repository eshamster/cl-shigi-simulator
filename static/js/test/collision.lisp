(in-package :cl-user)
(defpackage cl-shigi-simulator.static.js.test.collision
  (:use :cl
        :cl-ppcre
        :parenscript
        :ps-experiment
        :cl-web-2d-game
        :cl-shigi-simulator.static.js.tools 
        :cl-ps-ecs))
(in-package :cl-shigi-simulator.static.js.test.collision)

;; --- test col-cc --- ;;

(defun.ps make-circle ()
  (let* ((circle (make-ecs-entity))
         (r #y100))
    (add-entity-tag circle "circle")
    (add-ecs-component-list
     circle
     (make-point-2d :x #y300 :y #y600)
     (make-model-2d :model (make-wired-regular-polygon :n 60 :color 0xff0000 :r r)
                    :offset (make-vector-2d :x (* -1 r) :y (* -1 r))
                    :depth 1)
     (make-physic-circle :r r))
    (add-ecs-entity circle)))

;; --- test col-ct --- ;;

(defvar.ps+ *test-tri-pnts* (list (make-vector-2d :x #y-100 :y #y100)
                                  (make-vector-2d :x #y200 :y #y-100)
                                  (make-vector-2d :x #y100 :y #y300)))

(defun.ps make-triangle ()
  (let ((triangle (make-ecs-entity)))
    (add-entity-tag triangle "triangle")
    (add-ecs-component-list
     triangle
     (make-point-2d :x #y550 :y #y500)
     (make-model-2d :model (make-wired-polygon
                            :pnt-list (mapcar #'(lambda (vec) (with-slots (x y) vec (list x y)))
                                              *test-tri-pnts*)
                            :color 0xff4444)
                    :depth 1)
     (make-physic-triangle :pnt1 (nth 0 *test-tri-pnts*)
                           :pnt2 (nth 1 *test-tri-pnts*)
                           :pnt3 (nth 2 *test-tri-pnts*)))
    (add-ecs-entity triangle)))

;; --- test dist-to-line --- ;;
(defvar.ps+ *test-line-pnts* (list (make-vector-2d :x #y000 :y #y100)
                                   (make-vector-2d :x #y1333 :y #y700)))

(defun.ps make-test-line-for-calc-dist-to-line (scene)
  (scene.add (make-line :pos-a (list (vector-2d-x (car *test-line-pnts*))
                                     (vector-2d-y (car *test-line-pnts*)))
                        :pos-b (list (vector-2d-x (cadr *test-line-pnts*))
                                     (vector-2d-y (cadr *test-line-pnts*)))
                        :color 0x0000ff :z 1)))

(defun.ps test-dist-line (mouse-x mouse-y)
  (let ((hor-pnt-1 (make-vector-2d :x #y1333 :y #y500))
        (hor-pnt-2 (make-vector-2d :x #y0 :y #y500))
        (ver-pnt-1 (make-vector-2d :x #y666 :y #y0))
        (ver-pnt-2 (make-vector-2d :x #y666 :y #y1000))
        (mouse-pnt (make-vector-2d :x mouse-x :y mouse-y)))
    (labels ((calc-dist (pnt1 pnt2)
               (let ((dist (calc-dist-to-line mouse-pnt pnt1 pnt2)))
                 (dist.to-fixed 2))))
      (append-debug-text (+ "to blue-line="
                            (calc-dist (car *test-line-pnts*)
                                       (cadr *test-line-pnts*))))
      (append-debug-text (+ "to x-axis=" (calc-dist hor-pnt-1 hor-pnt-2)))
      (append-debug-text (+ "to y-axis=" (calc-dist ver-pnt-1 ver-pnt-2))))))

;; --- test dist-to-line-seg --- ;;
(defvar.ps+ *test-line-seg-pnts* (list (make-vector-2d :x #y800 :y #y500)
                                       (make-vector-2d :x #y1100 :y #y300)))

(defun.ps make-test-line-seg-for-calc-dist-to-line-seg (scene)
  (scene.add (make-line :pos-a (list (vector-2d-x (car *test-line-seg-pnts*))
                                     (vector-2d-y (car *test-line-seg-pnts*)))
                        :pos-b (list (vector-2d-x (cadr *test-line-seg-pnts*))
                                     (vector-2d-y (cadr *test-line-seg-pnts*)))
                        :color 0x00ffff :z 1)))

(defun.ps test-dist-line-seg (mouse-x mouse-y)
  (let ((mouse-pnt (make-vector-2d :x mouse-x :y mouse-y)))
    (labels ((calc-dist (pnt1 pnt2)
               (let ((dist (calc-dist-to-line-seg mouse-pnt pnt1 pnt2)))
                 (dist.to-fixed 2))))
      (append-debug-text (+ "to cyan-line-seg"
                            (calc-dist (car *test-line-seg-pnts*)
                                       (cadr *test-line-seg-pnts*)))))))

(defun.ps make-mouse-pointer ()
  (let ((pointer (make-ecs-entity))
        (r 30))
    (add-ecs-component-list
     pointer
     (make-point-2d)
     (make-model-2d :model (make-wired-regular-polygon :n 60 :color 0xff0000 :r r)
                    :offset (make-vector-2d :x (* -1 r) :y (* -1 r))
                    :depth 1)
     (make-script-2d :func (lambda (entity)
                             (with-ecs-components (point-2d) entity
                               (setf point-2d.x (get-mouse-x))
                               (setf point-2d.y (get-mouse-y)))
                             (test-dist-line (get-mouse-x) (get-mouse-y))
                             (test-dist-line-seg (get-mouse-x) (get-mouse-y))))
     (make-physic-circle :r r
                         :on-collision (lambda (mine target)
                                         (with-slots (tags) target
                                           (append-debug-text (+ "Collies to " (car tags)))))))
    (add-ecs-entity pointer)))

(defun.ps init (scene)
  (scene.add (make-line :pos-a (list #y1333 #y500) :pos-b (list 0 #y500) :color 0x00ff00 :z 1))
  (scene.add (make-line :pos-a (list #y666 #y0) :pos-b (list #y666 #y1000) :color 0x00ff00 :z 1))
  ;; for test of calc-dist-to-line
  (make-test-line-for-calc-dist-to-line scene)
  (make-test-line-seg-for-calc-dist-to-line-seg scene)
    
  (make-mouse-pointer)
  (make-circle)
  (make-triangle)
  
  (refresh-entity-display))

(defun.ps update ()
  (clear-debug-area)
  (process-input)
  (ecs-main))

(defun.ps main ()
  (start-game screen-width screen-height init update))

(defun js-main ()
  (with-use-ps-pack (:cl-shigi-simulator.static.js.2d-geometry
                     :cl-shigi-simulator.static.js.tools
                     :cl-shigi-simulator.static.js.input
                     :cl-shigi-simulator.static.js.basic-ecs
                     :this)
    (window.add-event-listener "mousemove" on-mouse-move-event)
    (window.add-event-listener "keydown" (lambda (e) (e.prevent-default)))
    (window.add-event-listener "DOMContentLoaded" main false))) 
