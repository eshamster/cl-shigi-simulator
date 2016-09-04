(in-package :cl-user)
(defpackage cl-shigi-simulator.templates.test-collision
  (:use :cl
        :cl-markup)
  (:import-from :cl-shigi-simulator.templates.layouts.three
                :with-three-layout)
  (:import-from :cl-shigi-simulator.static.js.utils
                :load-js))
(in-package :cl-shigi-simulator.templates.test-collision)

(defun render ()
  (with-three-layout (:title "Test collision" :js-name :test.collision :base-path "../")))
