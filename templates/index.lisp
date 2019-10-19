(defpackage cl-shigi-simulator/templates/index
  (:use :cl
        :cl-markup)
  (:import-from :cl-shigi-simulator/templates/layouts/defaults
                :with-default-layout)
  (:import-from :cl-shigi-simulator/static/js/utils
                :load-js))
(in-package :cl-shigi-simulator/templates/index)

(defun render ()
  (with-default-layout (:title "Welcome to Shigi simulator")
    (:div :id "main"
          (:h1 "Shigi simulator")
          (:div :id "to_shigi" (:a :href "shigi" "Start"))
          (:div :id "test_pages"
                (:div "Tests")
                (:div :class "test_url"
                      (:div (:a :href "test/ray" "Ray test"))
                      (:div (:a :href "test/orbiter" "Orbiter test")))))))
