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
           #:player-to-json
           ;#:player-collection
           ;#:save-player
           ;#:player-from-doc
           ;#:player-find-all
           ))

(defpackage #:foosman2-core.data
  (:use #:cl
        #:foosman2-core.model)
  (:export #:save-player
           #:player-find-all))