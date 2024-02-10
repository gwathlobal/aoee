;;;; mob.lisp

(in-package #:aoee-server)

(defconstant +max-ap+ 10)

;;------------------------
;; MOB-TYPE
;;------------------------

(defclass mob-type ()
  ((mob-type :initarg :mob-type :reader mob-type)
   (glyph-idx :initarg :glyph-idx :reader glyph-idx)
   (front-color :initform '(1.0 1.0 1.0 1.0) :initarg :front-color :reader front-color)
   (back-color :initform '(0.0 0.0 0.0 1.0) :initarg :back-color :reader back-color)
   
   (max-hp :initarg :max-hp :reader max-hp)))

(defun get-mob-type-by-id (mob-type-id)
  (gethash mob-type-id *mob-types*))

(defun set-mob-type (mob-type)
  (setf (gethash (mob-type mob-type) *mob-types*) mob-type))


;;------------------------
;; MOB
;;------------------------

(defclass mob ()
  ((id :initarg :id :reader id)
   (mob-type :initarg :mob-type :accessor mob-type)
   (x :initarg :x :initform 0 :accessor x)
   (y :initarg :y :initform 0 :accessor y)
   (ap :initform +max-ap+ :accessor ap)

   (hp :initarg :hp :accessor hp)
   (dead :initform nil :accessor dead)
   
   (visible-mobs :initform () :accessor visible-mobs)))

(defmethod yason:encode ((mob mob) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element :id (id mob))
      (yason:encode-object-element :mob-type (mob-type mob))
      (yason:encode-object-element :x (x mob))
      (yason:encode-object-element :y (y mob))
      (yason:encode-object-element :hp (hp mob)))))

(defun find-free-id (array)
  (loop for i from 0 below (length array)
        unless (aref array i)
        do (return-from find-free-id i))
  (adjust-array array (list (1+ (length array))))
  (1- (length array)))

(defun make-mob (mob-type)
  (let ((mob (make-instance 'mob 
                            :id (find-free-id *mobs*)
                            :mob-type mob-type 
                            :hp (max-hp (get-mob-type-by-id mob-type)))))
    (setf (aref *mobs* (id mob)) mob)
    mob))

(defun get-mob-by-id (id)
  (when (null id)
    (return-from get-mob-by-id nil))
  (aref *mobs* id))

(defun get-mob-type-of-mob (mob)
  (get-mob-type-by-id (mob-type mob)))

(defun make-act (mob ap-cost)
  (decf (ap mob) ap-cost))

(defun skip-turn-p (mob)
  (block nil
    (when (<= (ap mob) 0)
      (return t))
    (when (dead-p mob)
      (return t))
  
    nil))