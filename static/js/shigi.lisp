(defpackage cl-shigi-simulator/static/js/shigi
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :parenscript
        :cl-web-2d-game
        :cl-shigi-simulator/static/js/tools)
  (:export :make-shigi
           :shigi-part-p)
  (:import-from :cl-shigi-simulator/static/js/lazer
                :get-lazer-tag)
  (:import-from :cl-shigi-simulator/static/js/target
                :target-component
                :make-target-component
                :target-component-enable
                :get-target-tag)
  (:import-from :ps-experiment/common-macros
                :with-slots-pair))
(in-package :cl-shigi-simulator/static/js/shigi)

(enable-ps-experiment-syntax)

(defvar.ps+ *shigi-collision-targets* (list (get-lazer-tag)))

(defun.ps+ check-shigi-part (entity)
  (check-entity-tags entity :shigi-part))

(defun.ps+ get-shigi-part-enable (shigi-part)
  (let ((target (get-ecs-component 'target-component shigi-part)))
    (assert target)
    (target-component-enable target)))

(defun.ps+ set-shigi-part-enable (shigi-part enable)
  (let ((target (get-ecs-component 'target-component shigi-part)))
    (assert target)
    (setf (target-component-enable target) enable)
    (if enable
        (enable-model-2d shigi-part)
        (disable-model-2d shigi-part))))

(defun.ps+ toggle-all-shigi-parts ()
  (let ((enable t))
    (when (find-a-entity #'(lambda (entity)
                             (and (has-entity-tag entity :shigi-part)
                                  (get-shigi-part-enable entity))))
      (setf enable nil))
    (do-tagged-ecs-entities (part :shigi-part)
      (set-shigi-part-enable part enable))
    enable))

(defun.ps+ toggle-shigi-part (shigi-part)
  (check-shigi-part shigi-part)
  (let ((enable (get-shigi-part-enable shigi-part)))
    (set-shigi-part-enable shigi-part (not enable))))

(defun.ps+ toggle-shigi-part-by-mouse (shigi-part target)
  (when (and (= (get-left-mouse-state) :down-now)
             (has-entity-tag target :mouse))
    (toggle-shigi-part shigi-part)))

(defun.ps+ make-center-point-marker ()
  (let* ((marker (make-ecs-entity))
         (len (get-param :shigi :marker-size))
         (offset (* -1 (/ len 2))))
    (add-ecs-component-list
     marker
     (make-model-2d :model (make-wired-rect :width len :height len
                                            :color (get-param :shigi :marker-color))
                    :depth (get-depth :enemy)
                    :offset (make-point-2d :x offset :y offset))
     (make-point-2d))
    marker))

(defun.ps+ change-shigi-bit-speed (bit scale)
  (check-entity-tags bit :shigi-bit)
  (with-ecs-components (rotate-2d) bit
    (setf (rotate-2d-speed rotate-2d)
          (* (get-param :shigi :bit :rot-speed) scale))))

(defun.ps+ change-all-shigi-bits-speed (scale)
  (do-tagged-ecs-entities (bit :shigi-bit)
    (change-shigi-bit-speed bit scale)))

(defun.ps+ make-shigi-bits ()
  (let ((result '())
        (num-bit 4)
        (rot-speed (get-param :shigi :bit :rot-speed))
        (r (get-param :shigi :bit :r))
        (dist (get-param :shigi :bit :dist)))
    (dotimes (i num-bit)
      (let* ((bit (make-ecs-entity))
             (angle (* 2 PI i (/ 1 num-bit)))
             (point (make-point-2d)))
        (movef-vector-to-circle point dist angle)
        (add-entity-tag bit :shigi-part :shigi-bit (get-target-tag))
        (add-ecs-component-list
         bit
         (make-model-2d :model (make-wired-regular-polygon :r r :n 100
                                                           :color (get-param :shigi :color))
                        :depth (get-depth :enemy))
         (make-physic-circle :r r
                             :on-collision #'toggle-shigi-part-by-mouse
                             :target-tags *shigi-collision-targets*)
         point
         (make-rotate-2d :speed rot-speed
                         :angle angle
                         :radious dist)
         (make-target-component)
         (init-entity-params :color (nth i (get-param :color-chip :colors))
                             :display-name (+ "Bit" (1+ i))
                             :bit-id i))
        (push bit result)))
    result))

(defun.ps+ rotate-shigi-body (body)
  "The rotation of the shigi body is like a swing of a pendulum. The center of the gravity is the point of the player (only the difference of the angle affects. the distance doesn't). Then, the max rotation speed and the max rotation acceleration is limitted by constant numbers."
  (with-ecs-components (rotate-2d (body-point point-2d)) body
    (with-slots-pair ((speed) rotate-2d
                      (angle) body-point)
      (let ((center (find-a-entity-by-tag :shigi-center)))
        (if (get-entity-param center :body-rotate-p)
            (let* ((player (find-a-entity-by-tag :player))
                   (angle-to-player (vector-2d-angle
                                     (decf-vector-2d (clone-vector-2d (get-ecs-component 'point-2d player))
                                                  (get-ecs-component 'point-2d center)))))
              (labels ((round-by-abs (value max-value)
                         (max (* -1 max-value)
                              (min value max-value))))
                (incf speed
                      (round-by-abs (* (diff-angle angle-to-player (- angle (/ PI 2)))
                                       (get-param :shigi :body :rot-gravity))
                                    (get-param :shigi :body :max-rot-accell)))
                (setf speed (round-by-abs speed (get-param :shigi :body :max-rot-speed)))))
            ;; else
            (setf speed 0))))))

(defun.ps+ calc-average-point (pnt-list)
  (let ((result (list 0 0)))
    (dolist (pnt pnt-list)
      (incf (car result) (car pnt))
      (incf (cadr result) (cadr pnt)))
    (mapcar #'(lambda (x) (/ x (length pnt-list))) result)))

(defun.ps+ make-shigi-bodies ()
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
                                            :angle 0)))
          (add-entity-tag body :shigi-part :shigi-body (get-target-tag))
          (add-ecs-component-list
           body
           (make-target-component)
           (make-model-2d :model (make-wired-polygon
                                  :pnt-list modified-pnt-list
                                  :color (get-param :shigi :color))
                          :depth (get-depth :enemy)
                          :offset model-offset)
           (make-physic-polygon :pnt-list
                                (mapcar (lambda (pnt-by-list)
                                          (make-vector-2d :x (car pnt-by-list)
                                                          :y (cadr pnt-by-list)))
                                        modified-pnt-list)
                                :offset model-offset
                                :on-collision #'toggle-shigi-part-by-mouse
                                :target-tags *shigi-collision-targets*)
           (make-point-2d :x (car center) :y (cadr center))
           (make-rotate-2d :speed (get-param :shigi :body :max-rot-speed)
                           :radious (vector-2d-abs center-vec)
                           :angle (vector-2d-angle center-vec))
           (make-script-2d :func #'rotate-shigi-body)
           ;; TODO: parameterize 4 (which is the number of the bit)
           (init-entity-params :color (nth (+ i 4) (get-param :color-chip :colors))
                               :display-name (+ "Body_" (if (= i 0) "R" "L"))))
          (push body result))))
    result))

(defun.ps+ make-shigi-center ()
  (let ((center (make-ecs-entity)))
    (add-entity-tag center :shigi-center)
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
    (add-panel-button 'toggle-all-shigi-parts
                      :on-change (lambda (value)
                                   (declare (ignore value))
                                   (toggle-all-shigi-parts)))
    center))

(defun.ps+ make-shigi ()
  (let ((center (make-shigi-center))
        (bodies (make-shigi-bodies))
        (bit-list (make-shigi-bits)))
    (add-ecs-entity center)
    (dolist (body bodies)
      (add-ecs-entity body center)
      (add-ecs-entity (make-center-point-marker) body))
    (dolist (bit bit-list)
      (add-ecs-entity bit center)
      (add-ecs-entity (make-center-point-marker) bit)
      (when (oddp (get-entity-param bit :bit-id))
        (toggle-shigi-part bit)))))

;; --- tools --- ;;

(defun.ps+ shigi-part-p (entity)
  (has-entity-tag entity :shigi-part))
