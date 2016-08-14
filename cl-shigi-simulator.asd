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
               :parenscript)
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
                 (:file "index" :depends-on ("utils"))))
               (:module "templates"
                :components
                ((:file "index"))
                :depends-on ("templates/layouts" "static/js"))
               (:module "templates/layouts"
                :components
                ((:file "utils")
                 (:file "default" :depends-on ("utils")))))
  :description ""
  :in-order-to ((test-op (load-op cl-shigi-simulator-test))))
