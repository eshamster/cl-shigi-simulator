(in-package :cl-user)
(defpackage cl-shigi-simulator-asd
  (:use :cl :asdf))
(in-package :cl-shigi-simulator-asd)

(defsystem cl-shigi-simulator
  :version "0.1"
  :author "eshamster"
  :license ""
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
               :cl-ps-ecs)
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db"))
                 (:file "web" :depends-on ("view"))
                 (:file "view" :depends-on ("config"))
                 (:file "db" :depends-on ("config"))
                 (:file "config"))
                :depends-on ("templates"))
               (:module "static/js"
                :components
                ((:file "utils")
                 (:file "2d-geometry")
                 (:file "basic-components")
                 (:file "basic-ecs" :depends-on ("basic-components"))
                 (:file "tools")
                 (:file "input" :depends-on ("tools"))
                 (:file "player" :depends-on ("basic-ecs" "2d-geometry" "tools"))
                 (:file "shigi" :depends-on ("basic-ecs" "2d-geometry" "tools"))
                 (:file "shigi-simulator"
                        :depends-on ("utils" "basic-ecs" "tools" "input" "2d-geometry"))))
               (:module "static/js/test" 
                :components
                ((:file "collision"))
                :depends-on ("static/js"))
               (:module "templates"
                :components
                ((:file "index")
                 (:file "shigi")
                 (:file "test-collision"))
                :depends-on ("templates/layouts" "static/js" "static/js/test"))
               (:module "templates/layouts"
                :components
                ((:file "utils")
                 (:file "default" :depends-on ("utils")))))
  :description ""
  :in-order-to ((test-op (load-op cl-shigi-simulator-test))))
