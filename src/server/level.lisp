;;;; level.lisp

(in-package #:aoee-server)

(defparameter *max-x-level* 25)
(defparameter *max-y-level* 25)

(defclass level ()
  ((max-x :initform *max-x-level* :initarg :max-x :reader max-x)
   (max-y :initform *max-y-level* :initarg :max-y :reader max-y)
   (terrain :initform nil :type (or vector null))
   (memo :initform nil :type (or vectir null)) ; of type vector containing id of terrain,  mob type, item type,
                                               ; feature type, visibility flag, revealed flag, turn number
   (mobs :initform nil :type (or vector null))
   (mob-id-list :initform (make-list 0) :accessor mob-id-list :type list)
   
   (cur-turn :initform 0 :accessor cur-turn)))

(defmethod yason:encode ((level level) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element :max-x (max-x level))
      (yason:encode-object-element :max-y (max-y level))
      (yason:encode-object-element :cur-turn (cur-turn level))
      
      (let ((memo-to-be-encoded (make-list 0)))
        (with-slots (max-x max-y) level
          (dotimes (y max-y)
            (dotimes (x max-x)
              (let ((single-memo (get-memo-* level x y)))
                (push (alexandria:plist-hash-table (list :t (get-single-memo-terrain-id single-memo) 
                                                         :m (get-single-memo-mob-type-id single-memo)
                                                         :i (get-single-memo-item-type-id single-memo)
                                                         :f (get-single-memo-feature-type-id single-memo)
                                                         :v (if (get-single-memo-visibility single-memo)
                                                                :true
                                                                :false)
                                                         :r (if (get-single-memo-revealed single-memo)
                                                                :true
                                                                :false)
                                                         :n (get-single-memo-turn-number single-memo)))
                      memo-to-be-encoded)))))
        (yason:encode-object-element :memo (reverse memo-to-be-encoded))))))

(defun make-level ()
  (let ((level (make-instance 'level)))
    (with-slots (max-x max-y terrain memo mobs) level
      (setf terrain (make-array (* max-x max-y) :initial-element :terrain-dirt))
      (setf memo (make-array (* max-x max-y) :initial-element nil))
      (setf mobs (make-array (* max-x max-y) :initial-element nil))
      
      (dotimes (x max-x)
        (dotimes (y max-y)
          (set-memo-* level x y (create-single-memo nil nil nil nil nil nil 0)))))
    level))

(defun get-terrain-* (level x y)
  (with-slots (terrain max-x) level
    (aref terrain (+ x (* max-x y)))))

(defun get-terrain-type-* (level x y)
  (get-terrain-type-by-id (get-terrain-* level x y)))

(defun set-terrain-* (level x y terrain-id)
  (with-slots (terrain max-x) level
    (setf (aref terrain (+ x (* max-x y))) terrain-id)))

(defun get-memo-* (level x y)
  (with-slots (memo max-x) level
    (aref memo (+ x (* max-x y)))))

(defun set-memo-* (level x y single-memo)
  (with-slots (memo max-x) level
    (setf (aref memo (+ x (* max-x y))) single-memo)))

(defun create-single-memo (terrain-id mob-type-id item-type-id feature-type-id visibility revealed turn-number)
  (make-array 7 :initial-contents (list terrain-id mob-type-id item-type-id feature-type-id 
                                        visibility revealed turn-number)))

(defun get-single-memo-terrain-id (single-memo)
  (aref single-memo 0))

(defun get-single-memo-mob-type-id (single-memo)
  (aref single-memo 1))

(defun get-single-memo-item-type-id (single-memo)
  (aref single-memo 2))

(defun get-single-memo-feature-type-id (single-memo)
  (aref single-memo 3))

(defun get-single-memo-visibility (single-memo)
  (aref single-memo 4))

(defun get-single-memo-revealed (single-memo)
  (aref single-memo 5))

(defun get-single-memo-turn-number (single-memo)
  (aref single-memo 6))

(defun set-single-memo-* (level x y &key (terrain-id (get-single-memo-terrain-id (get-memo-* level x y)))
                                         (mob-type-id (get-single-memo-mob-type-id (get-memo-* level x y)))
                                         (item-type-id (get-single-memo-item-type-id (get-memo-* level x y)))
                                         (feature-type-id (get-single-memo-feature-type-id (get-memo-* level x y)))
                                         (visibility (get-single-memo-visibility (get-memo-* level x y)))
                                         (revealed (get-single-memo-revealed (get-memo-* level x y)))
                                         (turn-number (get-single-memo-turn-number (get-memo-* level x y))))
  (let ((single-memo (get-memo-* level x y)))
    (setf (aref single-memo 0) terrain-id)
    (setf (aref single-memo 1) mob-type-id)
    (setf (aref single-memo 2) item-type-id)
    (setf (aref single-memo 3) feature-type-id)
    (setf (aref single-memo 4) visibility)
    (setf (aref single-memo 5) revealed)
    (setf (aref single-memo 6) turn-number)))

(defun get-mob-* (level x y)
  (with-slots (mobs max-x) level
    (get-mob-by-id (aref mobs (+ x (* max-x y))))))

(defun set-mob-* (level x y mob)
  (with-slots (mobs max-x) level
    (setf (aref mobs (+ x (* max-x y))) (id mob))))

(defun set-mob-id-* (level x y mob-id)
  (with-slots (mobs max-x) level
    (setf (aref mobs (+ x (* max-x y))) mob-id)))

(defun add-mob-to-level (level mob)
  (with-slots (mob-id-list) level
    ;; remove the mob from its old place (if it is indeed there)
    (set-mob-* level (x mob) (y mob) mob)
    
    (when (not (find (id mob) mob-id-list))
      ;; make the player be always the first in the list
      (if (eq mob *player*)
          (push (id mob) mob-id-list)
          (if mob-id-list
              (push (id mob) (cdr (nthcdr 0 mob-id-list))) 
              (push (id mob) mob-id-list))))))

(defun remove-mob-from-level (level mob)
  (with-slots (mob-id-list) level
    (when (find (id mob) mob-id-list)
      (setf mob-id-list (remove (id mob) mob-id-list)))
    (when (eq mob (get-mob-* level (x mob) (y mob)))
      (set-mob-id-* level (x mob) (y mob) nil))))

(defun in-level-bounds-p (level x y)
  (and (>= x 0) (>= y 0) (< x (max-x level)) (< y (max-y level))))