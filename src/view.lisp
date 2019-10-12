(defpackage cl-shigi-simulator/src/view
  (:use :cl)
  (:import-from :cl-shigi-simulator/src/config
                :*template-directory*)
  (:import-from :caveman2
                :*response*
                :response-headers)
  (:import-from :datafly
                :encode-json)
  (:export :render
           :render-json))
(in-package :cl-shigi-simulator/src/view)

(defun render (template-name &rest rest)
  (check-type template-name keyword)
  (apply (intern "RENDER"
                 (string-upcase
                  (concatenate 'string
                               "cl-shigi-simulator/templates/"
                               (symbol-name template-name))))
         rest))

(defun render-json (object)
  (setf (getf (response-headers *response*) :content-type) "application/json")
  (encode-json object))
