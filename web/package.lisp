;;;; package.lisp

(defpackage #:foosman2-web.server
  (:use #:cl
        #:hunchentoot 
        #:cl-who
        #:cl-arrows
        #:parenscript
        #:smackjack 
        #:foosman2-core.model
        #:foosman2-core.data))