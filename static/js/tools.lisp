(in-package :cl-user)
(defpackage cl-shigi-simulator.static.js.tools
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :parenscript)
  (:export :create-html-element))
(in-package :cl-shigi-simulator.static.js.tools)

(enable-ps-experiment-syntax)

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
    (incf #j.*debug-area*.innerHTML# (+ text ":"))
))
