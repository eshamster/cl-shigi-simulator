(defpackage cl-shigi-simulator/templates/test-multiple
  (:use :cl
        :cl-markup)
  (:import-from :cl-shigi-simulator/templates/layouts/three
                :with-three-layout)
  (:import-from :cl-shigi-simulator/static/js/utils
                :load-js))
(in-package :cl-shigi-simulator/templates/test-multiple)

(defun render ()
  (with-three-layout (:title "Test multiple of line" :js-name :test/multiple :base-path "../")))
