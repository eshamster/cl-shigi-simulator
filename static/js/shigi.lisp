(in-package :cl-user)
(defpackage cl-shigi-simulator.static.js.shigi
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :parenscript
        :cl-web-2d-game
        :cl-shigi-simulator.static.js.tools)
  (:import-from :ps-experiment.common-macros
                :with-slots-pair)
  (:export :shigi-part-valid-p))
(in-package :cl-shigi-simulator.static.js.shigi)

(enable-ps-experiment-syntax)

(defun.ps+ check-shigi-part (entity)
  (check-entity-tags entity "shigi-part"))

(defun.ps toggle-shigi-part (shigi-part)
  (check-shigi-part shigi-part)
  (let ((enable (get-entity-param shigi-part :enable)))
    (if enable
        (progn (set-entity-param shigi-part :enable nil)
               (disable-model-2d shigi-part))
        (progn (set-entity-param shigi-part :enable t)
               (enable-model-2d shigi-part)))))

(defun.ps toggle-shigi-part-by-mouse (shigi-part target)
  (when (and (= (get-left-mouse-state) :down-now)
             (has-entity-tag target "mouse"))
    (toggle-shigi-part shigi-part)))

(defun.ps+ shigi-part-valid-p (shigi-part)
  (check-shigi-part shigi-part)
  (get-entity-param shigi-part :enable))

(defun.ps make-center-point-marker ()
  (let* ((marker (make-ecs-entity))
         (len (get-param :shigi :marker-size))
         (offset (* -1 (/ len 2))))
    (add-ecs-component-list
     marker
     (make-model-2d :model (make-wired-rect :width len :height len)
                    :depth (get-param :shigi :depth)
                    :offset (make-point-2d :x offset :y offset))
     (make-point-2d))
    marker))

(defun.ps change-shigi-bit-speed (bit scale)
  (check-entity-tags bit "shigi-bit")
  (with-ecs-components (rotate-2d) bit
    (setf (rotate-2d-speed rotate-2d)
          (* (get-param :shigi :bit :rot-speed) scale))))

(defun.ps change-all-shigi-bits-speed (scale)
  (do-tagged-ecs-entities (bit "shigi-bit")
    (change-shigi-bit-speed bit scale)))

(defun.ps make-shigi-bits ()
  (let ((result '())
        (num-bit 4)
        (rot-speed (get-param :shigi :bit :rot-speed))
        (r (get-param :shigi :bit :r))
        (dist (get-param :shigi :bit :dist)))
    (dotimes (i num-bit)
      (let* ((bit (make-ecs-entity))
             (angle (* 2 PI i (/ 1 num-bit)))
             (model-offset (make-vector-2d :x (* -1 r) :y (* -1 r) :angle 0))
             (point (make-point-2d)))
        (adjustf-point-by-rotate point dist angle)
        (add-entity-tag bit "shigi-part" "shigi-bit")
        (add-ecs-component-list
         bit
         (make-model-2d :model (make-wired-regular-polygon :r r :n 100
                                                           :color (get-param :shigi :color))
                        :depth (get-param :shigi :depth)
                        :offset model-offset)
         (make-physic-circle :r r
                             :on-collision #'toggle-shigi-part-by-mouse)
         point
         (make-rotate-2d :speed rot-speed
                         :angle angle
                         :radious dist)
         (init-entity-params :color (nth i (get-param :color-chip :colors))
                             :enable t)))
      (push bit result))
    result))

(defun.ps rotate-shigi-body (body)
  "The rotation of the shigi body is like a swing of a pendulum. The center of the gravity is the point of the player (only the difference of the angle affects. the distance doesn't). Then, the max rotation speed and the max rotation acceleration is limitted by constant numbers."
  (with-ecs-components (rotate-2d point-2d) body
    (with-slots-pair ((speed) rotate-2d
                      (angle) point-2d)
      (let ((center (find-a-entity-by-tag "shigi-center")))
        (if (get-entity-param center :body-rotate-p)
            (let* ((player (find-a-entity-by-tag "player"))
                   (angle-to-player (vector-angle
                                     (decf-vector (clone-vector (get-ecs-component 'point-2d player))
                                                  (get-ecs-component 'point-2d center)))))
              (labels ((round-by-abs (value max-value)
                         (max (* -1 max-value)
                              (min value max-value))))
                (incf speed
                      (round-by-abs (* (diff-angle angle-to-player (- angle (/ PI 2)))
                                       (get-param :shigi :body :rot-gravity))
                                    (get-param :shigi :body :max-rot-accell)))
                (let ((max-speed))
                  (setf speed (round-by-abs speed (get-param :shigi :body :max-rot-speed))))))
            ;; else
            (setf speed 0))))))

(defun.ps+ calc-average-point (pnt-list)
  (let ((result (list 0 0)))
    (dolist (pnt pnt-list)
      (incf (car result) (car pnt))
      (incf (cadr result) (cadr pnt)))
    (mapcar #'(lambda (x) (/ x (length pnt-list))) result)))

(defun.ps make-shigi-bodies ()
  (let* ((result '())
         (pnt-list '((#.#y0 #.#y76.8) (#.#y76.8 #.#y115.2)
                     (#.#y92.16 #.#y-57.6) (#.#y0 #.#y-144))))
    (labels ((reverse-list-by-x (pnt-list)
               (let ((result '()))
                 (dolist (pnt pnt-list)
                   (push (list (* (car pnt) -1) (cadr pnt)) result))
                 result)))
      (dotimes (i 2)
        (let* ((body (make-ecs-entity))
               (modified-pnt-list (if (= i 0)
                                      pnt-list
                                      (reverse-list-by-x pnt-list)))
               (center (calc-average-point modified-pnt-list))
               (center-vec (make-vector-2d :x (car center) :y (cadr center)))
               (model-offset (make-point-2d :x (* -1 (car center))
                                            :y (* -1 (cadr center))
                                            :angle 0))
               (rotate (make-rotate-2d :speed 0
                                       :radious (vector-abs center-vec)
                                       :angle (vector-angle center-vec))))
          (add-entity-tag body "shigi-part" "shigi-body")
          (add-ecs-component-list
           body
           (make-model-2d :model (make-wired-polygon
                                  :pnt-list modified-pnt-list
                                  :color (get-param :shigi :color))
                          :depth (get-param :shigi :depth)
                          :offset model-offset)
           (make-physic-polygon :pnt-list
                                (mapcar (lambda (pnt-by-list)
                                          (make-vector-2d :x (car pnt-by-list)
                                                          :y (cadr pnt-by-list)))
                                        modified-pnt-list)
                                :offset model-offset
                                :on-collision #'toggle-shigi-part-by-mouse)
           (make-point-2d :x (car center) :y (cadr center))
           rotate
           (make-script-2d :func #'rotate-shigi-body)
           ;; TODO: parameterize 4 (which is the number of the bit)
           (init-entity-params :color (nth (+ i 4) (get-param :color-chip :colors))
                               :enable t))
          (push body result))))
    result))

(defun.ps make-shigi-center ()
  (let ((center (make-ecs-entity)))
    (add-entity-tag center "shigi-center")
    (add-ecs-component-list
     center
     (make-point-2d :x (/ (get-param :play-area :width) 2) :y #y800)
     (make-script-2d :func (lambda (entity)
                             (change-all-shigi-bits-speed
                              (get-entity-param entity :bit-speed-scale))))
     (init-entity-params :bit-speed-scale 1
                         :body-rotate-p t))
    (add-panel-number 'bit-speed 1
                      :min 0 :max 1 :step 0.1
                      :on-change (lambda (value)
                                   (set-entity-param center :bit-speed-scale value)))
    (add-panel-bool 'body-rotate t
                    :on-change (lambda (value)
                                 (set-entity-param center :body-rotate-p value)))
    center))

(defun.ps make-shigi ()
  (let ((center (make-shigi-center))
        (bodies (make-shigi-bodies))
        (bit-list (make-shigi-bits)))
    (add-ecs-entity center)
    (dolist (body bodies)
      (add-ecs-entity body center)
      (add-ecs-entity (make-center-point-marker) body))
    (dolist (bit bit-list)
      (add-ecs-entity bit center)
      (add-ecs-entity (make-center-point-marker) bit))))
