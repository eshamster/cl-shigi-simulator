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
  (let* ((z 1000)
         (camera (new (#j.THREE.OrthographicCamera#
                       0 width height 0 0 (* z 2)))))
    (camera.position.set 0 0 z)
    camera))

;; - others -

(defun.ps start-game (screen-width screen-height init-function update-function)
  (init-stats)
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
               (renderer.render scene camera)
               (update-stats)
               (funcall update-function)))
      (render-loop))))

;; --- about screensize --- ;;

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defvar.ps+ screen-width 800)
  (defvar.ps+ screen-height 600)
  
  (defun.ps+ calc-absolute-length (relative-length)
    "Calculate an absolute length based on the screen height (1000 = screen-height)"
    (* relative-length screen-height 0.001))
  
  "Ex. '#y0.5' represents a half length of the screen height"
  (set-dispatch-macro-character
   #\# #\y
   #'(lambda (stream &rest rest)
       (declare (ignore rest))
       `(calc-absolute-length ,(read stream)))))

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
    (to-json '(:player (:speed #.#y4
                        :depth 100
                        :color #x000000
                        :ring-r #.#y70
                        :body-r #.#y7)
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
                            :size #.#y50))))

(defmacro.ps get-param (&rest keys)
  (labels ((rec (rest-keys result)
             (if rest-keys
                 (rec (cdr rest-keys)
                      (list '@ result (car rest-keys)))
                 result)))
    (rec keys '*all-params*)))

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

