;;;; aoee-server.asd

(asdf:defsystem #:aoee-server
  :description "Describe aoee-server here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (log4cl portal yason)
  :components ((:file "package")
               (:file "setup-third-party")
               (:file "globals")
               (:module "websocket"
                :components ((:file "package")
                             (:file "wrapper")
                             (:file "rwlock")))
               (:file "mob")
               (:file "terrain")
               (:file "level")
               (:file "attack")
               (:file "movement")
               (:file "los-fov")
               (:file "ai")
               (:module "data-gen"
                :components ((:file "package")
                             (:file "client-config")
                             (:file "generator")))
               (:file "init-terrain-types")
               (:file "init-mob-types")
               (:file "game-loop-state")
               (:file "aoee-server")))
