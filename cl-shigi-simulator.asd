(in-package :cl-user)
(defpackage cl-shigi-simulator-asd
  (:use :cl :asdf))
(in-package :cl-shigi-simulator-asd)

(defsystem cl-shigi-simulator
  :version "0.1"
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
               :cl-web-2d-game)
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
                 (:file "basic-ecs" :depends-on ("tools"))
                 (:file "tools")
                 (:file "shigi" :depends-on ("basic-ecs" "tools"))
                 (:file "lazer" :depends-on ("basic-ecs" "tools" "shigi"))
                 (:file "player" :depends-on ("basic-ecs" "tools" "shigi" "lazer"))
                 (:file "color-chip" :depends-on ("basic-ecs" "tools" "shigi"))
                 (:file "shigi-simulator"
                        :depends-on ("utils" "basic-ecs" "tools"))))
               (:module "static/js/test" 
                :components
                ((:file "collision")
                 (:file "ray"))
                :depends-on ("static/js"))
               (:module "templates"
                :components
                ((:file "index")
                 (:file "shigi")
                 (:file "test-collision")
                 (:file "test-ray"))
                :depends-on ("templates/layouts" "static/js" "static/js/test"))
               (:module "templates/layouts"
                :components
                ((:file "utils")
                 (:file "default" :depends-on ("utils"))
                 (:file "three" :depends-on ("utils")))
                :depends-on ("static/js")))
  :description ""
  :in-order-to ((test-op (load-op cl-shigi-simulator-test))))
