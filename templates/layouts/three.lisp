(in-package :cl-user)
(defpackage cl-shigi-simulator.templates.layouts.three
  (:use :cl
        :cl-markup)
  (:import-from :cl-shigi-simulator.templates.layouts.utils
                :with-markup-to-string)
  (:import-from :cl-shigi-simulator.static.js.utils
                :load-js)
  (:export :with-three-layout))
(in-package :cl-shigi-simulator.templates.layouts.three)

;; base-path requires a slash at last
;; Ex. NG: ".."
;;     OK: "../"
(defmacro with-three-layout ((&key title js-name (base-path "") (description nil)) &body body)
  `(with-markup-to-string
     (html5 (:head
             (:title ,title)
             (:meta :charset "UTF-8")
             (:script :src ,(merge-pathnames "js/copied/wtf-trace.js" base-path) nil)
             (:script :src ,(merge-pathnames "js/copied/dat.gui.min.js" base-path) nil)
             (:script :src "https://cdnjs.cloudflare.com/ajax/libs/three.js/r73/three.js" nil)
             (:script :src ,(merge-pathnames "js/copied/threex.keyboardstate.js" base-path) nil)
             (:script :src ,(merge-pathnames "js/copied/stats.js" base-path) nil)
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
             ,@body))))
