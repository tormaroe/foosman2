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
           #:player-points-v1))

(defpackage #:foosman2.data
  (:use #:cl 
        #:cl-mongo
        #:foosman2.model)
  (:export #:save-player
           #:get-players)) 

(defpackage #:foosman2
  (:use #:cl
        #:hunchentoot 
        #:cl-who
        #:parenscript
        #:smackjack 
        #:cl-mongo
        #:foosman2.model
        #:foosman2.data))