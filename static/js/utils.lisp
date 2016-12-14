(in-package :cl-user)
(defpackage cl-shigi-simulator.static.js.utils
  (:use :cl)
  (:export :load-js))
(in-package :cl-shigi-simulator.static.js.utils)

(defparameter *force-reload-js* t)

(defun split-path (package-name)
  ;; "some.package.name" -> ("some/package/" . "name")
  ;; "name" -> ("" . "name")
  (let ((splitted (mapcar #'reverse (ppcre:split
                                     "/"
                                     (reverse (ppcre:regex-replace-all
                                               "\\."
                                               package-name
                                               "/"))
                                     :limit 2))))
    (cons (if (second splitted)
              (concatenate 'string
                           (second splitted) "/")
              "")
          (car splitted))))

(defun make-full-path (path)
  (check-type path string)
  (asdf:system-relative-pathname :cl-shigi-simulator path))

(defun make-js-full-path (name)
  (check-type name string)
  (destructuring-bind (dir . file-name) (split-path name)
    (make-full-path (format nil "static/js/~A_~A.js" dir file-name))))

(defun make-js-load-path (name base-path)
  (check-type name string)
  (destructuring-bind (dir . file-name) (split-path name)
    (format nil "~Ajs/~A_~A.js"
            (if base-path
                (ppcre:regex-replace "/?$" base-path "/")
                "")
            dir file-name)))

(defun make-cl-path (name)
  (check-type name string)
  (destructuring-bind (dir . file-name) (split-path name)
    (make-full-path (format nil "static/js/~A~A.lisp" dir file-name))))

(defun is-js-older (name)
  (check-type name string)
  (let ((js-path (make-js-full-path name)))
    (or (not (probe-file js-path))
        (< (file-write-date js-path)
           (file-write-date (make-cl-path name))))))

(defun write-to-js-file (name)
  (with-open-file (out (make-js-full-path name)
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

(defun load-js (js-name &key (base-path nil))
  (check-type js-name keyword)
  (let ((name (string-downcase (symbol-name js-name))))
    (when (or *force-reload-js*
              (is-js-older name))
      (write-to-js-file name))
    (make-js-load-path name base-path)))
