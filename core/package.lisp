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
           #:player-points-v1-singles-part
           #:player-points-v1-doubles-part
           #:player-points-v1-singles
           #:player-points-v1-doubles
           #:player-points-v1-max
           #:player-points-v1-min
           #:player-points-v1-average
           #:player-points-v1-history
           #:player-badges
           #:player-match-count
           #:set-points-v1
           
           #:make-game-single
           #:game-single-timestamp
           #:game-single-winner
           #:game-single-looser
           
           #:make-game-double
           #:game-double-timestamp
           #:game-double-winner-player-1
           #:game-double-winner-player-2
           #:game-double-looser-player-1
           #:game-double-looser-player-2

           #:played-in-game-p))

(defpackage #:foosman2-core.points-v1
  (:use #:cl #:foosman2-core.model)
  (:export #:*initial-points*
           #:adjust-single-game
           #:adjust-double-game))

(defpackage #:foosman2-core.badges
  (:use #:cl #:foosman2-core.model)
  (:export #:award-badges
           #:badge-title
           #:badge-description
           #:badge-class))

(defpackage #:foosman2-core.data
  (:use #:cl
        #:chanl
        #:alexandria
        #:cl-arrows
        #:foosman2-core.model
        #:foosman2-core.badges)
  (:export #:initialize-event-processor
           #:load-events
           #:*event-log-pathname*
           #:command-add-player
           #:command-add-game-single
           #:command-add-game-double
           #:player-find-all
           #:player-by-name))
