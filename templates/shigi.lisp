(in-package :cl-user)
(defpackage cl-shigi-simulator.templates.shigi
  (:use :cl
        :cl-markup)
  (:import-from :cl-shigi-simulator.templates.layouts.three
                :with-three-layout)
  (:import-from :cl-shigi-simulator.static.js.utils
                :load-js))
(in-package :cl-shigi-simulator.templates.shigi)

(defun render ()
  (with-three-layout (:title "Shigi Simulator" :js-name :shigi-simulator
                      :description ((:div "操作説明:")
                                    (:div "・マウス&キーボード → 方向キー： 移動, C： 解放, パーツクリック: パーツのON/OFF")
                                    (:div "・タッチ → フリック：移動（終点で解放）, パーツタップ：パーツのON/OFF")))))
