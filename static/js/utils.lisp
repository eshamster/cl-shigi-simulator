(in-package :cl-user)
(defpackage cl-shigi-simulator.static.js.utils
  (:use :cl)
  (:export :load-js))
(in-package :cl-shigi-simulator.static.js.utils)

(defparameter *force-reload-js* t)

(defun make-full-path (path)
  (check-type path string)
  (asdf:system-relative-pathname :cl-shigi-simulator path))

(defun make-js-path (name &key (for-load nil))
  (check-type name string)
  (if for-load
      (format nil "js/_~A.js" name)
      (make-full-path (format nil "static/js/_~A.js" name))))

(defun make-cl-path (name)
  (check-type name string)
  (make-full-path (format nil "static/js/~A.lisp" name)))

(defun is-js-older (name)
  (check-type name string)
  (let ((js-path (make-js-path name)))
    (or (not (probe-file js-path))
        (< (file-write-date js-path)
           (file-write-date (make-cl-path name))))))

(defun write-to-js-file (name)
  (with-open-file (out (make-js-path name)
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (format t "(re-)load js: ~A" name)
    (format out
            (funcall (intern "JS-MAIN"
                             (string-upcase
                              (concatenate 'string
                                           "cl-shigi-simulator.static.js."
                                           name)))))))

(defun load-js (js-name)
  (check-type js-name keyword)
  (let ((name (string-downcase (symbol-name js-name))))
    (if (or *force-reload-js*
            (is-js-older name))
        (write-to-js-file name))
    (make-js-path name :for-load t)))
