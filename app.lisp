(defpackage cl-shigi-simulator/app
  (:use :cl)
  (:import-from :lack
                :builder)
  (:import-from :cl-ppcre
                :scan
                :regex-replace)
  (:import-from :cl-shigi-simulator/src/web
                :*web*)
  (:import-from :cl-shigi-simulator/src/config
                :config
                :productionp
                :*static-directory*))
(in-package :cl-shigi-simulator/app)

(builder
 (:static
  :path (lambda (path)
          (if (ppcre:scan "^(?:/images/|/css/|/js/|/robot\\.txt$|/favicon\\.ico$)" path)
              path
              nil))
  :root *static-directory*)
 (if (productionp)
     nil
     :accesslog)
 (if (getf (config) :error-log)
     `(:backtrace
       :output ,(getf (config) :error-log))
     nil)
 :session
 (if (productionp)
     nil
     (lambda (app)
       (lambda (env)
         (let ((datafly:*trace-sql* t))
           (funcall app env)))))
 *web*)
