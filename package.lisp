;;;; package.lisp


(defpackage #:foosman2.config
  (:use #:cl 
        #:cl-mongo))

(defpackage #:foosman2
  (:use #:cl
        #:hunchentoot 
        #:cl-who
        #:smackjack 
        #:cl-mongo))

