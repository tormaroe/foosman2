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
           
           #:make-game-single
           #:game-single-timestamp
           #:game-single-winner
           #:game-single-looser
           #:game-single-to-json
           
           #:make-game-double
           #:game-double-timestamp
           #:game-double-winner-player-1
           #:game-double-winner-player-2
           #:game-double-looser-player-1
           #:game-double-looser-player-2
           #:game-double-to-json))

(defpackage #:foosman2-core.points-v1
  (:use #:cl #:foosman2-core.model)
  (:export #:*initial-points*
           #:adjust-single-game
           #:adjust-double-game))

(defpackage #:foosman2-core.data
  (:use #:cl
        #:chanl
        #:alexandria
        #:cl-arrows
        #:foosman2-core.model)
  (:export #:initialize-event-processor
           #:command-add-player
           #:command-add-game-single
           #:command-add-game-double
           #:player-find-all
           #:player-by-name))