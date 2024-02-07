;;;; globals.lisp

(in-package #:aoee-server)

(defparameter *terrain-types* (make-hash-table :test 'eq))
(defparameter *mob-types* (make-hash-table :test 'eq))


(defparameter *mobs* (make-array 0 :adjustable t))

(defparameter *player* nil)

(defparameter *level* nil)