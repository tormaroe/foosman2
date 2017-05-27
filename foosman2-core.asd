;;;; foosman2-core.asd

(asdf:defsystem #:foosman2-core
  :description "Foosball management core system"
  :author "Torbjørn Marø <torbjorn.maro@gmail.com>"
  :license "MIT"
  :depends-on ()
  :serial t
  :components ((:file "core/package")
               (:file "core/config")
               (:file "core/model")
               (:file "core/data")))
