(in-package :cl-user)
(defpackage cl-shigi-simulator/src/web
  (:use :cl
        :caveman2
        :cl-shigi-simulator/src/config
        :cl-shigi-simulator/src/view
        :cl-shigi-simulator/src/db
        :datafly
        :sxql)
  (:export :*web*))
(in-package :cl-shigi-simulator/src/web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Routing rules

(defroute "/" ()
  (render :index))

(defroute "/shigi" ()
  (render :shigi))

(defroute "/test/ray" ()
  (render :test-ray))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
