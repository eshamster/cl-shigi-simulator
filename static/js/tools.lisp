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
              :rot-speed (* PI 12/180)
              :min-speed #y10
              :max-speed #y45
              :accell #y3)
      :lazer-state (:start (:time 8))
      :lazer-maker (:min-speed #y15
                  :max-speed #y30
                  :min-angle (* PI 60/180)
                  :max-angle (* PI 78/180)
                  :half-num 4
                  :first-offset (:x #y35 :y 0)
                  :dummy-target1 (:x #y184 :y #y112))
      :shigi (:depth 50
              :color #x112222
              :marker-color #x666666
              :marker-size #y10
              :body (:max-rot-speed 0.0175
                     :max-rot-accell 8.72e-4
                     :rot-gravity 0.002)
              :bit (:r #y30
                    :dist #y173
                    :rot-speed -0.0272))
      :cursor (:color #x771111)
      :color-chip (:colors (list #xeeaaee
                                 #x999aff
                                 #xfcb879
                                 #x9bee57
                                 #x77bd77
                                 #xaaf7ff)
                   :depth -50
                   :size #y40))))

(defmacro.ps+ get-param (&rest keys)
  `(get-layered-hash *global-params* ,@keys))

;; --- for initialize --- ;;

(defun.ps start-game (&key screen-width screen-height
                           (camera-offset-x 0) (camera-offset-y 0)
                           (init-function (lambda (scene) nil))
                           (update-function (lambda () nil)))
  (init-stats)
  (init-gui)
  (start-2d-game :screen-width screen-width
                 :screen-height screen-height
                 :camera (init-camera camera-offset-x camera-offset-y
                                      screen-width screen-height)
                 :rendered-dom (document.query-selector "#renderer")
                 :stats-dom (document.query-selector "#renderer")
                 :monitoring-log-dom (document.query-selector "#debug")
                 :event-log-dom (document.query-selector "#log")
                 :init-function (lambda (scene)
                                  (register-default-systems scene)
                                  (funcall init-function scene))
                 :update-function update-function))

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
