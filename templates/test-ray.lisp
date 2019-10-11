(in-package :cl-user)
(defpackage cl-shigi-simulator/templates/test-ray
  (:use :cl
        :cl-markup)
  (:import-from :cl-shigi-simulator/templates/layouts/three
                :with-three-layout)
  (:import-from :cl-shigi-simulator/static/js/utils
                :load-js))
(in-package :cl-shigi-simulator/templates/test-ray)

(defun render ()
  (with-three-layout (:title "Test ray of line" :js-name :test/ray :base-path "../")))
