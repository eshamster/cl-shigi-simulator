(defpackage cl-shigi-simulator/templates/test-orbiter
  (:use :cl
        :cl-markup)
  (:import-from :cl-shigi-simulator/templates/layouts/three
                :with-three-layout)
  (:import-from :cl-shigi-simulator/static/js/utils
                :load-js))
(in-package :cl-shigi-simulator/templates/test-orbiter)

(defun render ()
  (with-three-layout (:title "Test orbiter of lazer" :js-name :test/orbiter :base-path "../")))
