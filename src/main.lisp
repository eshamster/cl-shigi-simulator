(in-package :cl-user)
(defpackage cl-shigi-simulator
  (:use :cl)
  (:import-from :cl-shigi-simulator.config
                :config)
  (:import-from :clack
                :clackup)
  (:export :start
           :stop))
(in-package :cl-shigi-simulator)

(defvar *appfile-path*
  (asdf:system-relative-pathname :cl-shigi-simulator #P"app.lisp"))

(defvar *handler* nil)

(defun start (&rest args &key server port (address "0.0.0.0") debug &allow-other-keys)
  (declare (ignore server port debug))
  (stop)
  (when *handler*
    (restart-case (error "Server is already running.")
      (restart-server ()
        :report "Restart the server"
        (stop))))
  (let ((clack-args (if (getf args :address)
                        args
                        (append args (list :address address)))))
    (setf *handler*
          (apply #'clackup *appfile-path* clack-args))))

(defun stop ()
  (when *handler*
    (prog1
        (clack:stop *handler*)
      (setf *handler* nil))))
