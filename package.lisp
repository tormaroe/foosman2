;;;; package.lisp

(defpackage #:foosman2.config
  (:use #:cl 
        #:cl-mongo))

(defpackage #:foosman2.model
  (:use #:cl)
  (:export #:make-player
           #:player-name
           #:player-singles-won
           #:player-singles-lost
           #:player-doubles-won
           #:player-doubles-lost
           #:player-points-v1
           #:player-to-json
           #:player-collection
           #:save-player
           #:player-from-doc
           #:player-find-all))

(defpackage #:foosman2
  (:use #:cl
        #:hunchentoot 
        #:cl-who
        #:parenscript
        #:smackjack 
        #:cl-mongo
        #:foosman2.model))