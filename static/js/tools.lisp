(in-package :cl-user)
(defpackage cl-shigi-simulator.static.js.tools
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :parenscript)
  (:export :create-html-element
           :get-param
           :with-trace))
(in-package :cl-shigi-simulator.static.js.tools)

(enable-ps-experiment-syntax)

;; --- for profiling --- ;;

;; Note: this is depend on Web Tracing Framework (wtf-trace.js)

(defmacro.ps with-trace (title &body body)
  `(let ((scope (#j.WTF.trace.enterScope# ,title)))
     ,@body
     (#j.WTF.trace.leaveScope# scope ,title)))
(defmacro with-trace (title &body body)
  "(dummy)"
  (declare (ignore title))
  `(progn ,@body))

;; --- about screensize --- ;;

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defvar.ps+ screen-width 800)
  (defvar.ps+ screen-height 600)
  ;; width : height = 3 : 4
  (defvar.ps+ play-area-height 570)
  (defvar.ps+ play-area-width (/ (* play-area-height 3) 4))
  
  (defun.ps+ calc-absolute-length (relative-length base-length)
    "Calculate an absolute length based on the screen height (1000 = base-length)"
    (* relative-length base-length 0.001))
  
  "Ex1. '#y0.5' represents a half length of the play-area height."
  "Ex2. '#ys0.5' represents a half length of the screen height."
  (set-dispatch-macro-character
   #\# #\y
   #'(lambda (stream &rest rest)
       (declare (ignore rest))
       (case (peek-char nil stream)
         (#\s (read-char stream)
            `(calc-absolute-length ,(read stream) screen-height))
         (t `(calc-absolute-length ,(read stream) play-area-height))))))

;; --- constant value manager --- ;;

(defun.ps to-json (list)
  (labels ((rec (list table)
             (when (> (length list) 0)
               (let ((key (car list))
                     ;; TODO: check value != null
                     (value (cadr list))
                     (rest (cddr list)))
                 (setf (gethash key table)
                       (if (instanceof value -array)
                           (rec value (make-hash-table))
                           value))
                 (rec rest table)))
             table))
    (let ((table (make-hash-table)))
      (rec list table)
      table)))

(defvar.ps *all-params*
    (to-json '(:play-area (:x #.#y326.7544
                           :y #.#y25
                           :width #.play-area-width
                           :height #.play-area-height)
               :player (:speed #.#y4
                        :depth 100
                        :color #x000000
                        :ring-r #.#y70
                        :body-r #.#y7)
               :lazer (:depth 70
                       :tail-length 20
                       :max-speed #.#y45
                       :max-rot-speed #.(* PI 28/180))
               :shigi (:depth 50
                       :color #x112222
                       :marker-size #.#y10
                       :body (:max-rot-speed 0.0175
                              :max-rot-accell 8.72e-4
                              :rot-gravity 0.002)
                       :bit (:r #.#y30
                             :dist #.#y173
                             :rot-speed -0.0272))
               :cursor (:color #x771111)
               :color-chip (:colors (0 #x7777bd
                                     1 #xee579b
                                     2 #xbd7777
                                     3 #x9bee57
                                     4 #x77bd77
                                     5 #x579bee)
                            :depth -50
                            :size #.#y40))))

(defmacro.ps get-param (&rest keys)
  (labels ((rec (rest-keys result)
             (if rest-keys
                 (rec (cdr rest-keys)
                      (list '@ result (car rest-keys)))
                 result)))
    (rec keys '*all-params*)))

;; --- for initialize --- ;;

;; - stats -

(defvar.ps *stats* nil)

(defun.ps init-stats ()
  (setf *stats* (new (-stats)))
  (let ((stats *stats*))
    (stats.set-mode 0)
    (with-slots (position left top) stats.dom-element.style
      (setf position "absolute")
      (setf left "0px")
      (setf top "0px"))
    ((@ (document.get-element-by-id "stats-output") append-child) stats.dom-element)
    stats))

(defun.ps update-stats ()
  (*stats*.update))

;; - camera -

(defun.ps init-camera (width height)
  (let* ((x (get-param :play-area :x))
         (y (get-param :play-area :y))
         (z 1000)
         (camera (new (#j.THREE.OrthographicCamera#
                       (* x -1) (- width x)
                       (- height y) (* y -1)
                       0 (* z 2)))))
    (camera.position.set 0 0 z)
    camera))

;; - others -

(defun.ps start-game (screen-width screen-height init-function update-function)
  (init-stats)
  (init-gui)
  (let* ((scene (new (#j.THREE.Scene#)))
         (camera (init-camera screen-width screen-height))
         (renderer (new #j.THREE.WebGLRenderer#)))
    (register-default-systems scene)
    (renderer.set-size screen-width screen-height)
    ((@ ((@ document.query-selector) "#renderer") append-child) renderer.dom-element)
    (let ((light (new (#j.THREE.DirectionalLight# 0xffffff))))
      (light.position.set 0 0.7 0.7)
      (scene.add light))

    (funcall init-function scene)
    (labels ((render-loop ()
               (request-animation-frame render-loop)
               (with-trace "render"
                 (renderer.render scene camera))
               (update-stats)
               (with-trace "update"
                 (funcall update-function))))
      (render-loop))))

;; --- html --- ;;

(defmacro.ps create-html-element (tag &key id html class)
  `(let ((element (document.create-element ,tag)))
     ,(when id
            `(setf element.id ,id))
     ,(when class
            (if (atom class)
                `(element.class-list.add ,class)
                `(dolist (cls ,class)
                   (element.class-list.add cls))))
     ,(when html
            `(setf #j.element.innerHTML# ,html))
     element))

(defun.ps refresh-entity-display ()
  (let ((tree (document.query-selector "#entity-tree"))
        (test-obj (make-point-2d)))
    (do-ecs-entities entity
      (let* ((id (ecs-entity-id entity))
             (entity-div (create-html-element
                          "dt"
                          :id (concatenate 'string "Entity" id)
                          :html (concatenate 'string "Entity (ID: " id ")")
                          :class '("entity" "tree"))))
        (tree.append-child entity-div)
        (do-ecs-components-of-entity (component entity)
          (let ((component-div (create-html-element
                                "dd"
                                :html component.constructor.name
                                :class '("component" "tree"))))
            (tree.append-child component-div)))))))

;; --- debug write --- ;;

(defvar.ps *debug-area* nil)

(defun.ps clear-debug-area ()
  (setf *debug-area* (document.query-selector "#debug"))
  (setf #j.*debug-area*.innerHTML# "Debug: "))

(defun.ps append-debug-text (text)
  (when *debug-area*
    (incf #j.*debug-area*.innerHTML# (+ text "<br>"))))

