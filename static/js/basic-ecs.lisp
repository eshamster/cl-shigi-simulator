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
                          (cl-shigi-simulator.static.js.tools:with-trace "script_system"
                            (with-ecs-components (script-2d) entity
                              (funcall (script-2d-func script-2d) entity))))))))

(defun.ps register-default-systems (scene)
  (register-ecs-system "move2d" (make-move-system))
  (register-ecs-system "rotate2d" (make-rotate-system))
  (register-ecs-system "script2d" (make-script-system))
  (register-ecs-system "collision" (make-collision-system))
  (register-ecs-system "draw2d" (init-draw-model-system scene)))
