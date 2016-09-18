(in-package :cl-user)
(defpackage cl-shigi-simulator.static.js.basic-ecs
  (:use :cl
        :cl-ppcre
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game
        :parenscript))
(in-package :cl-shigi-simulator.static.js.basic-ecs)

(enable-ps-experiment-syntax)

(defstruct.ps+ (script-2d (:include ecs-component)) (func (lambda (entity) entity)))

;; --- systems --- ;;

(defstruct.ps
    (draw-model-system
     (:include ecs-system
               (target-component-types '(point-2d model-2d))
               (process (lambda (entity)
                          (with-ecs-components (model-2d point-2d) entity
                            (let ((new-pos (calc-global-point entity
                                                              (model-2d-offset model-2d))))
                              (with-slots (model) model-2d
                                (model.position.set
                                 (point-2d-x new-pos)
                                 (point-2d-y new-pos)
                                 (model-2d-depth model-2d)) 
                                (setf model.rotation.z (point-2d-angle new-pos))))))))))

(defstruct.ps+
    (move-system
     (:include ecs-system
               (target-component-types '(point-2d speed-2d))
               (process (lambda (entity)
                          (with-ecs-components (point-2d speed-2d) entity
                            (incf-vector point-2d speed-2d)))))))

(defstruct.ps+
    (rotate-system
     (:include ecs-system
               (target-component-types '(point-2d rotate-2d))
               (process (lambda (entity)
                          (with-ecs-components (point-2d) entity
                            (do-ecs-components-of-entity (rotate-2d entity)
                              (when (rotate-2d-p rotate-2d)
                                (with-slots (speed (rot-angle angle) radious) rotate-2d
                                  (incf-rotate-diff point-2d radious
                                                    rot-angle speed)
                                  (with-slots (angle) point-2d
                                    (incf angle speed))
                                  (incf rot-angle speed))))))))))

(defstruct.ps+
    (script-system
     (:include ecs-system
               (target-component-types '(script-2d))
               (process (lambda (entity)
                          (with-ecs-components (script-2d) entity
                            (funcall (script-2d-func script-2d) entity)))))))

(defun.ps register-default-systems (scene)
  (register-ecs-system "draw2d"
                       (make-draw-model-system
                        :add-entity-hook (lambda (entity)
                                           (with-ecs-components (model-2d) entity
                                             (scene.add (model-2d-model model-2d))))
                        :delete-entity-hook (lambda (entity)
                                              (with-ecs-components (model-2d) entity
                                                (scene.remove (model-2d-model model-2d))))))
  (register-ecs-system "move2d" (make-move-system))
  (register-ecs-system "rotate2d" (make-rotate-system))
  (register-ecs-system "script2d" (make-script-system))
  (register-ecs-system "collision" (make-collision-system))
  (register-ecs-system "color-chip" (make-color-chip-system)))
