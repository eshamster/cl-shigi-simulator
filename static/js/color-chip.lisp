(in-package :cl-user)
(defpackage cl-shigi-simulator.static.js.color-chip
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :parenscript
        :cl-web-2d-game
        :cl-shigi-simulator.static.js.shigi
        :cl-shigi-simulator.static.js.tools)
  (:import-from :ps-experiment.common-macros
                :with-slots-pair))
(in-package :cl-shigi-simulator.static.js.color-chip)

(enable-ps-experiment-syntax)

(defun.ps get-chip-num-x ()
  (floor (1+ (/ screen-width (get-param :color-chip :size)))))

(Defun.ps get-chip-num-y ()
  (floor (1+ (/ screen-height (get-param :color-chip :size)))))

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
         (depth (get-param :color-chip :depth)))
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
          (pair-list (make-shigi-part-point-pairs)))
      (setf geometry.colors-need-update t)
      (dotimes (y (get-chip-num-y))
        (setf (point-2d-y buffer-pnt) (* (+ 0.5 y) size))
        (dotimes (x (get-chip-num-x))
          (setf (point-2d-x buffer-pnt) (* (+ 0.5 x) size))
          (let* ((nearest-part (get-nearest-shigi-part pair-list buffer-pnt)))
            (set-chip-color geometry x y
                            (if nearest-part
                                (get-entity-param nearest-part :color)
                                #xffffff))))))))

(defun.ps generate-color-grid ()
  (let ((grid (make-ecs-entity)))
    (add-ecs-component-list
     grid
     (make-point-2d)
     (make-model-2d :model (make-color-grid-mesh))
     (make-script-2d :func process-color-grid))
    (add-ecs-entity grid)))

;; -----------------
;; TODO: Move these to a more proper package because the player also uses this.
(defun.ps+ make-shigi-part-point-pairs ()
  (let ((result '()))
    (do-tagged-ecs-entities (entity "shigi-part")
      (when (shigi-part-valid-p entity)
        (push (list entity (calc-global-point entity))
              result)))
    result))

(defun.ps+ get-nearest-shigi-part (shigi-parts-points pnt)
  (let ((min-len -1)
        (nearest-part nil))
    (dolist (part-pnt-pair shigi-parts-points)
      (let ((dist (calc-dist-p2 pnt (cadr part-pnt-pair))))
        (when (or (< min-len 0)
                  (< dist min-len))
          (setf min-len dist)
          (setf nearest-part (car part-pnt-pair)))))
    nearest-part))
;; -----------------

