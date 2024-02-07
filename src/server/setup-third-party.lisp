;;;; setup-third-party.lisp

(in-package #:aoee-server)

(defparameter *current-dir* (asdf:system-source-directory :aoee-server))

(defun str-to-keyword (string)
  (if (or (null string)
          (string= string ""))
      nil
      (intern (string-upcase string) '#:keyword)))

(log:config :sane :console :daily (merge-pathnames (make-pathname :name "log.txt") *current-dir*) 
            :pattern "%D - %p (%c{1}) - %m%n" :info)

(setf yason:*parse-object-key-fn* #'str-to-keyword)
(setf yason:*symbol-key-encoder* #'yason:encode-symbol-as-lowercase)
(setf yason:*symbol-encoder* #'yason:encode-symbol-as-lowercase)


