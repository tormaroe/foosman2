;;;; package.lisp

(defpackage #:foosman2-core.config
  (:use #:cl))

(defpackage #:foosman2-core.model
  (:use #:cl)
  (:export #:make-player
           #:player-name
           #:player-singles-won
           #:player-singles-lost
           #:player-doubles-won
           #:player-doubles-lost
           #:player-points-v1
           #:player-to-json))

(defpackage #:foosman2-core.data
  (:use #:cl
        #:chanl
        #:alexandria
        #:cl-arrows
        #:foosman2-core.model)
  (:export #:initialize-event-processor
           #:command-add-player
           #:player-find-all))