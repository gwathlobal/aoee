;;;; terrain.lisp

(in-package #:aoee-server)

;;------------------------
;; TERRAIN-TYPE
;;------------------------

(defclass terrain-type ()
  ((id :initarg :id :reader id)
   (glyph-idx :initarg :glyph-idx :reader glyph-idx)
   (front-color :initform '(1.0 1.0 1.0 1.0) :initarg :front-color :reader front-color)
   (back-color :initform '(0.0 0.0 0.0 1.0) :initarg :back-color :reader back-color)
   (name :initform "No name terrain" :initarg :name :reader name)
   (trait :initform (make-hash-table) :reader trait)))

(defmethod initialize-instance :after ((terrain-type terrain-type) 
                                       &rest keys
                                       &key trait-blocks-move trait-blocks-move-floor trait-blocks-vision 
                                       trait-blocks-vision-floor trait-blocks-projectiles  
                                       trait-blocks-projectiles-floor trait-slope-up trait-slope-down 
                                       trait-not-climable trait-light-source trait-blocks-sound 
                                       trait-blocks-sound-floor trait-water (trait-move-cost-factor 1)
                                       trait-openable-door trait-openable-window trait-flammable trait-can-jump-over
                                       trait-can-have-rune trait-can-switch-light)

  (with-slots (trait) terrain-type
    ;; go through supplied keys
    (loop for (key value) on keys by #'cddr 
          do
             (setf (gethash key trait) value))
    ;; specifically add keys with default values, 
    ;; because otherwise non-supplied key with a default value will not show up in &rest
    (setf (gethash :trait-move-cost-factor trait) trait-move-cost-factor)
    ))

(defun get-terrain-type-by-id (terrain-type-id)
  (gethash terrain-type-id *terrain-types*))

(defun set-terrain-type (terrain-type)
  (setf (gethash (id terrain-type) *terrain-types*) terrain-type))

(defun get-terrain-type-trait (terrain-type-id key)
  (gethash key (trait (get-terrain-type-by-id terrain-type-id))))


