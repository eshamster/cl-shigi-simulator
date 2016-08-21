(in-package :cl-user)
(defpackage cl-shigi-simulator.templates.shigi
  (:use :cl
        :cl-markup)
  (:import-from :cl-shigi-simulator.templates.layouts.utils
                :with-markup-to-string)
  (:import-from :cl-shigi-simulator.static.js.utils
                :load-js))
(in-package :cl-shigi-simulator.templates.shigi)

(defun render ()
  (with-markup-to-string
    (html5 (:head
            (:title "Test ThreeJs")
            (:meta :charset "UTF-8")
            (:script :src "https://cdnjs.cloudflare.com/ajax/libs/three.js/r73/three.min.js" nil)
            (:script :src "js/copied/threex.keyboardstate.js" nil)
            (:script :src "js/copied/stats.js" nil)
            (:script :src (load-js :shigi-simulator) nil)
            (:link :rel "stylesheet" :type "text/css" :href "css/shigi.css"))
           (:body
            (:div (:a :href "/" "Top"))
            (:br)
            (:div :id "stats-output")
            (:div :id "renderer" nil)
            (:div :id "debug" "Debug用領域")
            (:br)
            (:div "Entityのリスト"
                  (:dl :id "entity-tree"))))))
