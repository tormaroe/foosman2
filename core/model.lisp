;;;; model.lisp

(in-package #:foosman2-core.model)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   MODELS
;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct player
  name
  (singles-won 0 :type fixnum)
  (singles-lost 0 :type fixnum)
  (doubles-won 0 :type fixnum)
  (doubles-lost 0 :type fixnum)
  (points-v1 0 :type fixnum)
  (points-v1-max 0 :type fixnum)
  (points-v1-history ()))

(defstruct game-single 
  timestamp 
  winner 
  looser)

(defstruct game-double 
  timestamp 
  winner-player-1 
  winner-player-2 
  looser-player-1 
  looser-player-2)


(defun set-points-v1 (player points)
  (push points (player-points-v1-history player))
  (setf (player-points-v1 player) points)
  (setf (player-points-v1-max player) 
        (max points
             (player-points-v1-max player)))
  player)