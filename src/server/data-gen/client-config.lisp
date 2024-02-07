;;;; client-config.lisp

(in-package #:aoee-server/data-gen)

(defclass godot-client-config ()
  ((path-to-client-root :initform 
                        (merge-pathnames "client/"
                                         (uiop:pathname-parent-directory-pathname aoee-server::*current-dir*))
                        :initarg :path-to-client-root
                        :reader path-to-client-root)
   ;; definitions
   (path-to-definitions :initform #P"assets/definitions/" :initarg :path-tp-definitions :reader path-to-definitions)
   (def-terrains-dir :initform "tile/" :initarg :def-terrains-dir :reader def-terrains-dir)
   (def-mobs-dir :initform "tile/" :initarg :def-mobs-dir :reader def-mobs-dir)
   ;; spritesheet
   (spritesheet-max-cols :initform 25 :initarg :spritesheet-max-cols :reader spritesheet-max-cols)
   (spritesheet-tile-w :initform 16 :initarg :spritesheet-tile-w :reader spritesheet-tile-w)
   (spritesheet-tile-h :initform 16 :initarg :spritesheet-tile-h :reader spritesheet-tile-h)))

(defparameter *godot-client-config* (make-instance 'godot-client-config))

(defun get-path-to-terrain-defs (&optional (config *godot-client-config*))
  (merge-pathnames (def-terrains-dir config)
                   (merge-pathnames (path-to-definitions config)
                                    (path-to-client-root config))))

(defun get-path-to-mob-defs (&optional (config *godot-client-config*))
  (merge-pathnames (def-mobs-dir config)
                   (merge-pathnames (path-to-definitions config)
                                    (path-to-client-root config))))