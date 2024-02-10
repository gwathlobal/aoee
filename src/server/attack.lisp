;;;; movement.lisp

(in-package #:aoee-server)

(defun melee-attack (mob target)
  (when (or (dead-p mob) (dead-p target))
    (return-from melee-attack))
  
  (log:info "Mob [~A] melee attacks mob [~A]."
            (id mob) (id target))
  
  (inflict-dmg mob target (list :min 1 :max 2)))

(defun inflict-dmg (mob target dmg-info)
  (destructuring-bind (&key (min 0) (max 0) &allow-other-keys) dmg-info
    (let ((dmg (+ min (random (+ max 1)))))

      (when (= dmg 0)
        (return-from inflict-dmg))

      (log:info "Mob [~A] inflicts ~A dmg to mob [~A]." (id mob) dmg (id target))
      
      (decf (hp target) dmg)
      
      (when (and (<= (hp target) 0)
                 (not (dead target)))
        (make-dead target)))))

(defun dead-p (mob)
  (or (dead mob)
      (<= (hp mob) 0)))

(defun make-dead (mob)
  (setf (dead mob) t)
  (remove-mob-from-level *level* mob)
  (log:info "Mob [~A] dies." (id mob)))