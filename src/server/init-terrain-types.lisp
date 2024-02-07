;;;; init-terrain-types.lisp

(in-package #:aoee-server)

(defun define-terrain (terrain-type)
  (set-terrain-type terrain-type)
  (aoee-server/data-gen:gen-terrain-template terrain-type))

(define-terrain (make-instance 'terrain-type 
                               :id :terrain-dirt
                               :glyph-idx 95
                               :name "Dirt Terrain"))

(define-terrain (make-instance 'terrain-type 
                               :id :terrain-wall
                               :glyph-idx 96
                               :front-color '(1.0 1.0 1.0 1.0)
                               :back-color '(1.0 1.0 1.0 1.0)
                               :name "Stone Wall" 
                               :trait-blocks-move t
                               :trait-blocks-vision t))