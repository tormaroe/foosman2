;;;; package.lisp

(defpackage #:foosman2-web.server
  (:use #:cl
        #:hunchentoot 
        #:cl-who
        #:cl-arrows
        #:parenscript
        #:smackjack 
        #:foosman2-core.model
        #:foosman2-core.badges
        #:foosman2-core.data)
  (:import-from #:alexandria #:with-input-from-file)
  (:export #:start-foosman2
           #:start-foosman2-daemon))