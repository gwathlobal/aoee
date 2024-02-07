;;;; los-fov.lisp

(in-package #:aoee-server)

(defun get-distance (sx sy tx ty)
  (declare (type fixnum sx sy tx ty))
  (sqrt (+ (* (- sx tx) (- sx tx)) (* (- sy ty) (- sy ty)))))

(defun line-of-sight (x0 y0 x1 y1 func)
  (declare (optimize (speed 3))
           (type fixnum x0 x1 y0 y1)
           (type function func))
  (let* ((dx (abs (- x1 x0)))
         (dy (abs (- y1 y0)))
         (sx (if (< x0 x1) 1 -1))
         (sy (if (< y0 y1) 1 -1))
         (dm (max dx dy))
         (i dm))
    (declare (type fixnum dx dy sx sy dm i))
    (setf x1 (truncate dm 2) y1 (truncate dm 2))
    (loop with prev-cell = nil
          while (and (not (< i 0))
                     (not (eq (funcall func x0 y0 prev-cell) 'exit)))
          do
             (decf i)
             (setf prev-cell (list x0 y0))
             (decf x1 dx) (decf y1 dy)
             (when (< x1 0)
               (incf x1 dm)
               (incf x0 sx))
             (when (< y1 0)
               (incf y1 dm)
               (incf y0 sy)))))

(defun draw-fov (cx cy r func)
  (declare (optimize (speed 3)))
  (declare (type fixnum cx cy r)
           (type function func))
  (let ((target-cells nil))
    (loop for i of-type fixnum from 0 to 360 by 1
          for tx of-type fixnum = (+ cx (round (* r (cos (* i (/ pi 180))))))
          for ty of-type fixnum = (- cy (round (* r (sin (* i (/ pi 180 ))))))
          do
             (unless (find (cons tx ty) target-cells :test #'equal)
               (push (cons tx ty) target-cells)))
    (loop for (tx . ty) in target-cells do
             (line-of-sight cx cy tx ty func))))

(defun update-visible-map-player (&optional (player *player*) (level *level*))
  ;; make the whole level invisible
  (with-slots (max-x memo) level
    (dotimes (n (array-dimension memo 0))
      (multiple-value-bind (y x) (truncate n max-x)
        (set-single-memo-* level x y :visibility nil))))

  (setf (visible-mobs player) ())
  
  ;; make some of level visible according to fov
  (draw-fov (x player) (y player) 8 #'(lambda (dx dy prev-cell)
                                        (declare (ignore prev-cell))
                                        (let ((exit-result t))
                                          (block nil
                                            (when (> (get-distance (x player) (y player) dx dy) 8)
                                              (setf exit-result 'exit)
                                              (return))

                                            (when (not (in-level-bounds-p level dx dy))
                                              (setf exit-result 'exit)
                                              (return))

                                            (let ((tmob (get-mob-* level dx dy)))
                                              (unless (null tmob)
                                                (pushnew (id tmob) (visible-mobs player)))
                                              
                                              ;; TODO here
                                              (set-single-memo-* level dx dy
                                                                 :terrain-id (get-terrain-* level dx dy)
                                                                 :mob-type-id (if tmob
                                                                                  (mob-type tmob)
                                                                                  nil)
                                                                 :item-type-id nil
                                                                 :feature-type-id nil
                                                                 :visibility t
                                                                 :revealed t
                                                                 :turn-number 0))
                                            
                                            (when (get-terrain-type-trait (get-terrain-* level dx dy) 
                                                                          :trait-blocks-vision)
                                              (setf exit-result 'exit)
                                              (return)))
                                          exit-result))))

(defun update-visible-map-mob (mob &optional (level *level*))
  (setf (visible-mobs mob) ()))

(defun update-visible-map (mob &optional (level *level*))
  (if (eq mob *player*)
      (update-visible-map-player mob level)
      (update-visible-map-mob mob level)))