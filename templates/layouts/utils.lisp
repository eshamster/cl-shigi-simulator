(defpackage cl-shigi-simulator/templates/layouts/utils
  (:use :cl
        :cl-markup)
  (:export :with-markup-to-string))
(in-package :cl-shigi-simulator/templates/layouts/utils)

(defmacro with-markup-to-string (&body body)
  (let ((str (gensym)))
    `(with-output-to-string (,str)
       (let ((cl-markup:*output-stream* ,str))
                  ,@body))))
