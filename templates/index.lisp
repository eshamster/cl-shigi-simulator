(in-package :cl-user)
(defpackage cl-shigi-simulator.templates.index
  (:use :cl
        :cl-markup)
  (:import-from :cl-shigi-simulator.templates.layouts.defaults
                :with-default-layout)
  (:import-from :cl-shigi-simulator.static.js.utils
                :load-js))
(in-package :cl-shigi-simulator.templates.index)

(defun render ()
  (with-default-layout (:title "Welcome to Shigi simulator")
    (:div :id "main"
          (:div "Welcome to Shigi simulator" "!")
          (:div :id "to_shigi" (:a :href "shigi" "Start")))))
