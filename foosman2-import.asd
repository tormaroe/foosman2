;;;; foosman2-import.asd

(asdf:defsystem #:foosman2-import
  :description "Foosball management legacy data import system"
  :author "Torbjørn Marø <torbjorn.maro@gmail.com>"
  :license "MIT"
  :depends-on (#:log4cl
               #:xmls
               #:alexandria
               #:chanl
               #:cl-arrows
               #:cl-ppcre
               #:foosman2-core)
  :serial t
  :components ((:file "import/legacy")))