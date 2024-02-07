;;;; ai.lisp

(in-package #:aoee-server)

(defun make-rnd-move (mob &optional (level *level*))
  (loop with dirs = '(:n :s :w :e :nw :sw :ne :se :c)
        for dir in dirs
        for can-move = (mob-can-move-p mob level dir)
        if (eq (first can-move) :free) collect dir into avail-dirs
        finally (mob-move mob level (alexandria:random-elt avail-dirs) :mob-can-move-p-result '(:free))))

(defun ai-function (mob)
  (make-rnd-move mob))

