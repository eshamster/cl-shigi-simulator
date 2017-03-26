(in-package :cl-user)
(defpackage cl-shigi-simulator.templates.layouts.three
  (:use :cl
        :cl-markup)
  (:import-from :cl-shigi-simulator.templates.layouts.utils
                :with-markup-to-string)
  (:import-from :cl-shigi-simulator.static.js.utils
                :load-js)
  (:import-from :cl-web-2d-game
                :make-src-list-for-script-tag
                :ensure-js-files)
  (:export :with-three-layout))
(in-package :cl-shigi-simulator.templates.layouts.three)

(defparameter *application-root* (asdf:system-source-directory :cl-shigi-simulator))
(defparameter *js-download-dir* (merge-pathnames #P"static/js/copied/" *application-root*))

;; base-path requires a slash at last
;; Ex. NG: ".."
;;     OK: "../"
(defmacro with-three-layout ((&key title js-name (base-path "") (description nil)) &body body)
  `(progn
     (ensure-js-files *js-download-dir*)
     (with-markup-to-string
       (html5 (:head
               (:title ,title)
               (:meta :charset "UTF-8")
               (dolist (js-src (make-src-list-for-script-tag
                                ,(merge-pathnames "js/copied/" base-path)))
                 (markup (:script :src js-src nil)))
               (:script :src (load-js ,js-name :base-path ,base-path) nil)
               (:link :rel "stylesheet" :type "text/css"
                      :href ,(merge-pathnames "css/shigi.css" base-path)))
              (:body
               (:div (:a :href "/" "Top"))
               (:div :id "description"
                     ,@description)
               (:br)
               (:div :id "stats-output")
               (:div :id "renderer" nil)
               (:div :id "debug" "(for Debug)")
               (:div (:pre :id "log" "(for Log)"))
               (:br)
               (:div "Entityのリスト"
                     (:dl :id "entity-tree"))
               ,@body)))))
