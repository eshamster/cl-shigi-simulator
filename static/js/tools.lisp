(in-package :cl-user)
(defpackage cl-shigi-simulator.static.js.tools
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game
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

;; --- global parameters --- ;;

(defvar.ps+ *global-params*
    (convert-to-layered-hash
     (:play-area (:x #y326.7544
                  :y #y25
                  :width #.play-area-width
                  :height #.play-area-height)
      :player (:speed #y8.33
               :depth 100
               :color #x000000
               :ring-r #y70
               :body-r #y7)
      :lazer (:depth 70
              :tail-length 16
              :max-speed #y45
              :max-rot-speed (* PI 28/180))
      :all-lazer (:min-angle (* PI 10/180)
                  :max-angle (* PI 50/180)
                  :half-num 6
                  :first-offset (:x #y35 :y 0))
      :shigi (:depth 50
              :color #x112222
              :marker-size #y10
              :body (:max-rot-speed 0.0175
                     :max-rot-accell 8.72e-4
                     :rot-gravity 0.002)
              :bit (:r #y30
                    :dist #y173
                    :rot-speed -0.0272))
      :cursor (:color #x771111)
      :color-chip (:colors (list #x7777bd
                                 #xee579b
                                 #xbd7777
                                 #x9bee57
                                 #x77bd77
                                 #x579bee)
                   :depth -50
                   :size #y40))))

(defmacro.ps+ get-param (&rest keys)
  `(get-layered-hash *global-params* ,@keys))

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
    (chain (document.get-element-by-id "stats-output")
           (append-child stats.dom-element))
    stats))

(defun.ps update-stats ()
  (*stats*.update))

;; - others -

(defun.ps start-game (&key screen-width screen-height
                           (camera-offset-x 0) (camera-offset-y 0)
                           (init-function (lambda (scene) nil))
                           (update-function (lambda () nil)))
  (init-stats)
  (init-gui)
  (let* ((scene (new (#j.THREE.Scene#)))
         (camera (init-camera camera-offset-x camera-offset-y
                              screen-width screen-height))
         (renderer (new #j.THREE.WebGLRenderer#)))
    (register-default-systems scene)
    (renderer.set-size screen-width screen-height)
    (chain (document.query-selector "#renderer")
           (append-child renderer.dom-element))
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

(defvar.ps+ *log-area* nil)
(defvar.ps+ *log-text-list* '())
(defvar.ps+ *max-log-count* 5)

(defun.ps push-log-text (text)
  (unless *log-area*
    (setf *log-area* (document.query-selector "#log")))
  (push text *log-text-list*)
  (when (> (length *log-text-list*) *max-log-count*)
    (setf *log-text-list* (subseq *log-text-list* 0 *max-log-count*)))
  (let ((log ""))
    ;; A newer log comes to upper.
    (dolist (one-line *log-text-list*)
      (setf log (+ log one-line "<br>")))
    (setf #j.*log-area*.innerHTML# log)))
