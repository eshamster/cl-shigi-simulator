(in-package :cl-user)
(defpackage cl-shigi-simulator.static.js.index
  (:use :cl
        :parenscript))
(in-package :cl-shigi-simulator.static.js.index)

(defun js-main ()
  (ps (alert "Hello Parenscript!!")))
