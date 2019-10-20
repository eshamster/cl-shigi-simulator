(defsystem cl-shigi-simulator
  :version "0.1"
  :class :package-inferred-system
  :author "eshamster"
  :license "LLGPL"
  :depends-on (:clack
               :lack
               :caveman2
               :envy
               :cl-ppcre
               :uiop

               ;; for @route annotation
               :cl-syntax-annot

               ;; HTML Template
               :cl-markup
               :djula

               ;; for DB
               :datafly
               :sxql

               ;; for JavaScript
               :ps-experiment
               :cl-ps-ecs
               :cl-web-2d-game

               ;; entrypoints
               :cl-shigi-simulator/app
               :cl-shigi-simulator/src/main
               :cl-shigi-simulator/templates/index
               :cl-shigi-simulator/templates/shigi
               :cl-shigi-simulator/templates/test-ray
               :cl-shigi-simulator/templates/test-orbiter
               :cl-shigi-simulator/templates/test-multiple
               :cl-shigi-simulator/static/js/shigi-simulator
               :cl-shigi-simulator/static/js/test/ray
               :cl-shigi-simulator/static/js/test/orbiter
               :cl-shigi-simulator/static/js/test/multiple)
  :description ""
  :in-order-to ((test-op (load-op cl-shigi-simulator-test))))
