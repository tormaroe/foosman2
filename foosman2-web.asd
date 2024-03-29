;;;; foosman2-core.asd

(asdf:defsystem #:foosman2-web
  :description "Foosball management web application"
  :author "Torbjørn Marø <torbjorn.maro@gmail.com>"
  :license "MIT"
  :depends-on (#:log4cl
               #:alexandria
               #:hunchentoot 
               #:cl-who
               #:parenscript
               #:smackjack
               #:cl-arrows
               #:foosman2-core)
  :serial t
  :components ((:file "web/package")
               (:file "web/server")))