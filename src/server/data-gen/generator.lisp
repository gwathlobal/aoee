;;;; generator.lisp

(in-package #:aoee-server/data-gen)

(defun gen-rect2-dimensions (idx max-cols tile-w tile-h)
  (multiple-value-bind (r c) (truncate idx max-cols)
    (format nil "~A, ~A, ~A, ~A" (* c tile-w) (* tile-h r) tile-w tile-h)))

(defun gen-color-values (color)
  (format nil "~{~A~^, ~}" color))

(defun gen-from-template (template-path result-path &rest rest)
  (let* ((template (alexandria:read-file-into-string (merge-pathnames template-path aoee-server::*current-dir*)))
         (result (apply #'format (append (list nil template) rest))))
    (alexandria:write-string-into-file result result-path :if-exists :supersede)))

(defun get-filename-from-keyword (keyword)
  (concatenate 'string (string-downcase (substitute #\_ #\- (symbol-name keyword))) ".tres"))

(defun gen-terrain-template (terrain-type)
  (gen-from-template "data-gen/tile-template"
                     (merge-pathnames (get-filename-from-keyword (aoee-server::id terrain-type))
                                      (get-path-to-terrain-defs))
                     (gen-rect2-dimensions (aoee-server::glyph-idx terrain-type) 
                                           (spritesheet-max-cols *godot-client-config*)
                                           (spritesheet-tile-w *godot-client-config*)
                                           (spritesheet-tile-h *godot-client-config*))
                     (string-downcase (symbol-name (aoee-server::id terrain-type)))
                     (gen-color-values (aoee-server::front-color terrain-type))
                     (gen-color-values (aoee-server::back-color terrain-type))))

(defun gen-mob-template (mob-type)
  (gen-from-template "data-gen/tile-template"
                     (merge-pathnames (get-filename-from-keyword (aoee-server::mob-type mob-type))
                                      (get-path-to-mob-defs))
                     (gen-rect2-dimensions (aoee-server::glyph-idx mob-type) 
                                           (spritesheet-max-cols *godot-client-config*)
                                           (spritesheet-tile-w *godot-client-config*)
                                           (spritesheet-tile-h *godot-client-config*))
                     (string-downcase (symbol-name (aoee-server::mob-type mob-type)))
                     (gen-color-values (aoee-server::front-color mob-type))
                     (gen-color-values (aoee-server::back-color mob-type))))