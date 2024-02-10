;;;; init-mob-types.lisp

(in-package #:aoee-server)

(defun define-mob (mob-type)
  (set-mob-type mob-type)
  (aoee-server/data-gen:gen-mob-template mob-type))

(define-mob (make-instance 'mob-type 
                           :mob-type :mob-type-player
                           :glyph-idx 32
                           :max-hp 10))

(define-mob (make-instance 'mob-type 
                           :mob-type :mob-type-imp
                           :glyph-idx 73
                           :front-color '(1.0 0 0 1.0)
                           :max-hp 3))
                               