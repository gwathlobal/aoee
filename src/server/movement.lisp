;;;; movement.lisp

(in-package #:aoee-server)

(defun dir-to-xy (dir)
  (case dir
    (:n (values 0 -1))
    (:s (values 0 1))
    (:w (values -1 0))
    (:e (values 1 0))
    (:ne (values 1 -1))
    (:nw (values -1 -1))
    (:se (values 1 1))
    (:sw (values -1 1))
    (:c (values 0 0))
    (t (progn
         (log:error "Unexpected :dir value, got ~A." dir)
         (values 0 0)))))

(defun mob-can-move-p (mob level dir)
  (multiple-value-bind (dx dy) (dir-to-xy dir)
    (let* ((x (+ (x mob) dx))
           (y (+ (y mob) dy))
           (terrain-id (if (in-level-bounds-p level x y)
                           (get-terrain-* level x y)
                           (return-from mob-can-move-p '(:out-of-bounds))))
           (tmob (get-mob-* level x y)))

      (when (and (not (null tmob))
                 (not (eq mob tmob)))
        (return-from mob-can-move-p `(:mob ,(id tmob))))
      
      (when (get-terrain-type-trait terrain-id :trait-blocks-move)
        (return-from mob-can-move-p `(:obstacle ,terrain-id)))

      '(:free))))

(defun mob-move (mob level dir &key mob-can-move-p-result (do-act t))
  (unless mob-can-move-p-result
    (setf mob-can-move-p-result (mob-can-move-p mob level dir)))
  
  (multiple-value-bind (dx dy) (dir-to-xy dir)
    (let* ((x (+ (x mob) dx))
           (y (+ (y mob) dy))
           (result (first mob-can-move-p-result)))

      (case result
        ((:obstacle :out-of-bounds) (progn
                                      (log:info "Mob [~A] is unable to move to (~A, ~A), obstacle [~A]." 
                                                (id mob) x y (if (> (length mob-can-move-p-result) 1)
                                                                 (second mob-can-move-p-result)
                                                                 nil))
                                      (return-from mob-move nil)))
        (:mob (progn
                (log:info "Mob [~A] is unable to move to (~A, ~A), another mob [~A]." 
                          (id mob) x y (second mob-can-move-p-result))
                (return-from mob-move nil))))

      
      (remove-mob-from-level level mob)
      
      (incf (x mob) dx)
      (incf (y mob) dy)
      
      (add-mob-to-level level mob)

      (when do-act
        (make-act mob +max-ap+))
      
      t)))