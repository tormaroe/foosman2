;;;; foosman2-core.asd

(asdf:defsystem #:foosman2-web
  :description "Foosball management web application"
  :author "Torbjørn Marø <torbjorn.maro@gmail.com>"
  :license "MIT"
  :depends-on (#:hunchentoot 
               #:cl-who
               #:parenscript
               #:smackjack
               #:foosman2-core)
  :serial t
  :components ((:file "web/package")
               (:file "web/server")))