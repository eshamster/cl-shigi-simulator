(in-package :cl-user)
(defpackage cl-shigi-simulator-test-asd
  (:use :cl :asdf))
(in-package :cl-shigi-simulator-test-asd)

(defsystem cl-shigi-simulator-test
  :author "eshamster"
  :license ""
  :depends-on (:cl-shigi-simulator
               :prove)
  :components ((:module "t"
                :components
                ((:file "cl-shigi-simulator"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
