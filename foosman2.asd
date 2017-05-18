;;;; foosman2.asd

(asdf:defsystem #:foosman2
  :description "Foosball management web application"
  :author "Torbjørn Marø <torbjorn.maro@gmail.com>"
  :license "MIT"
  :depends-on (#:hunchentoot 
               #:cl-who
               #:smackjack 
               #:cl-mongo)
  :serial t
  :components ((:file "package")
               (:file "config")
               (:file "foosman2")))

