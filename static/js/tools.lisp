(in-package :cl-user)
(defpackage cl-shigi-simulator.static.js.tools
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :parenscript)
  (:export :create-html-element
           :get-param))
(in-package :cl-shigi-simulator.static.js.tools)

(enable-ps-experiment-syntax)

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
                        :ring-r #.#y70
                        :body-r #.#y7))))

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
    (incf #j.*debug-area*.innerHTML# (+ text ":"))))

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

;; --- others --- ;;

;; TODO: Move this to cl-ps-ecs

(defun.ps+ add-ecs-component-list (entity &rest component-list)
  (dolist (component component-list)
    (add-ecs-component component entity)))
