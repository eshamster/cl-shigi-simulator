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
  (with-default-layout (:title "Welcome to Caveman2")
    (:div :id "main"
          "Welcome to " (:a :href "http://8arrow.org/caveman/" "Caveman2") "!")
    (:script :src (load-js :index) nil)))
