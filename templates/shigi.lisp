(in-package :cl-user)
(defpackage cl-shigi-simulator.templates.shigi
  (:use :cl
        :cl-markup)
  (:import-from :cl-shigi-simulator.templates.layouts.three
                :with-three-layout)
  (:import-from :cl-shigi-simulator.static.js.utils
                :load-js))
(in-package :cl-shigi-simulator.templates.shigi)

(defun render ()
  (with-three-layout (:title "Shigi Simulator" :js-name :shigi-simulator)))
