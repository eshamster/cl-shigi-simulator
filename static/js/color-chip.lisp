(defpackage cl-shigi-simulator/static/js/color-chip
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :parenscript
        :cl-web-2d-game
        :cl-shigi-simulator/static/js/shigi
        :cl-shigi-simulator/static/js/tools)
  (:export :generate-color-grid)
  (:import-from :cl-shigi-simulator/static/js/target
                :get-target-pnt-pairs
                :get-nearest-target)
  (:import-from :ps-experiment/common-macros
                :with-slots-pair))
(in-package :cl-shigi-simulator/static/js/color-chip)

(enable-ps-experiment-syntax)

(defun.ps+ get-chip-num-x ()
  (floor (1+ (/ (get-param :play-area :width) (get-param :color-chip :size)))))

(defun.ps+ get-chip-num-y ()
  (floor (1+ (/ (get-param :play-area :height) (get-param :color-chip :size)))))

(defun.ps set-chip-color (grid-geometry x-index y-index color)
  "Note: A chip consists of 2 triangles. Caller should set true to geometry.colorsNeedUpdate."
  (let ((index (* 2 (+ x-index
                       (* (get-chip-num-x) y-index))))
        (faces grid-geometry.faces))
    (dotimes (i 2)
      ((@ faces (+ index i) color set-hex) color))))

;; TODO: Hide the three.js layer.
(defun.ps make-color-grid-mesh ()
  (let* ((geometry (new (#j.THREE.Geometry#)))
         (size (get-param :color-chip :size))
         (num-x (get-chip-num-x))
         (num-y (get-chip-num-y))
         (depth (get-depth :color-chip)))
    (with-slots (vertices colors faces) geometry
      (dotimes (y (1+ num-y))
        (dotimes (x (1+ num-x))
          (vertices.push (new (#j.THREE.Vector3# (* x size) (* y size) depth)))))
      (dotimes (y num-y)
        (dotimes (x num-x)
          (let ((index (+ x (* y (1+ num-x))))) ;; left-down of the rectangle
            (faces.push (new (#j.THREE.Face3# index (1+ index) (+ index num-x 1))))
            (faces.push (new (#j.THREE.Face3# (+ index num-x 1) (1+ index) (+ index num-x 2)))))))))
  (new (#j.THREE.Mesh# geometry
                       (new (#j.THREE.MeshBasicMaterial#
                             (create vertex-colors #j.THREE.FaceColors#))))))

(defun.ps process-color-grid (grid)
  (with-ecs-components (model-2d) grid
    (let ((geometry model-2d.model.geometry)
          (size (get-param :color-chip :size))
          (buffer-pnt (make-point-2d))
          (target-pnt-pairs (get-target-pnt-pairs)))
      (setf geometry.colors-need-update t)
      (dotimes (y (get-chip-num-y))
        (setf (point-2d-y buffer-pnt) (* (+ 0.5 y) size))
        (dotimes (x (get-chip-num-x))
          (setf (point-2d-x buffer-pnt) (* (+ 0.5 x) size))
          (flet ((get-color ()
                   (if (get-entity-param grid :enable-chips)
                       (let ((nearest-part (get-nearest-target buffer-pnt target-pnt-pairs)))
                         (if nearest-part
                             (get-entity-param nearest-part :color)
                             #xffffff))
                       #xffffff)))
            (set-chip-color geometry x y (get-color))))))))

(defun.ps generate-color-grid ()
  (let ((grid (make-ecs-entity)))
    (add-ecs-component-list
     grid
     (make-point-2d)
     (make-model-2d :model (make-color-grid-mesh))
     (make-script-2d :func process-color-grid)
     (init-entity-params :enable-chips t))
    (add-panel-bool 'color-grid t
                    :on-change (lambda (value)
                                 (set-entity-param grid :enable-chips value)))
    (add-ecs-entity grid)))

