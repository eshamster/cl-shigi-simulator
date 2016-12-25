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
                            (do-ecs-components-of-entity (rotate entity
                                                                 :component-type 'rotate-2d)
                              (rotatef-point-by point-2d rotate))))))))

(defun.ps register-default-systems (scene)
  (register-ecs-system "move2d" (make-move-system))
  (register-ecs-system "rotate2d" (make-rotate-system))
  (init-default-systems :scene scene))
