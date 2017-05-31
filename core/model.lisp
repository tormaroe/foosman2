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
  (points-v1-singles-part 0 :type fixnum)
  (points-v1-doubles-part 0 :type fixnum)
  (points-v1-singles 0 :type fixnum)
  (points-v1-doubles 0 :type fixnum)
  (points-v1-max 0 :type fixnum)
  (points-v1-min 0 :type fixnum)
  (points-v1-average 0 :type fixnum)
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

(defun average (list) 
  (floor (reduce #'+ list)
         (length list)))

(defun set-points-v1 (player points)
  (push points (player-points-v1-history player))
  (setf (player-points-v1 player) points)
  (setf (player-points-v1-max player) 
        (max points
             (player-points-v1-max player)))
  (setf (player-points-v1-min player) 
        (min points
             (player-points-v1-min player)))
  (setf (player-points-v1-average player)
        (average (player-points-v1-history player)))
  player)