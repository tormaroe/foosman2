;;;; model.lisp

(in-package #:foosman2-core.model)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   MODELS
;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; - PLAYER --------------------------------------------------------------------------------------

(defstruct player
  name
  (last-active 0 :type fixnum)
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
  (points-v1-history ())
  (badges ())
  (team-mates (make-hash-table :test #'equal)))

(defun update-last-active-timestamp (timestamp px)
  (loop for p in px
     do (setf (player-last-active p) timestamp)))

(defun update-team-mate (player team-mate change winners)
  (let* ((team-mate-name (player-name team-mate))
         (team-mates (player-team-mates player))
         (team-mate (or (gethash team-mate-name team-mates)
                        (make-team-mate :name team-mate-name))))
    (push change
          (team-mate-score-changes team-mate))
    (if winners
      (incf (team-mate-won team-mate))
      (incf (team-mate-lost team-mate)))
    (setf (gethash team-mate-name team-mates)
          team-mate)))

(defun update-team-mates (p1 p2 score-change &key winners)
  (let ((score-change (if winners 
                        score-change
                        (* -1 score-change))))
    (update-team-mate p1 p2 score-change winners)
    (update-team-mate p2 p1 score-change winners)))

(defun player-active-p (p)
  (<= (- (get-universal-time)
         (player-last-active p))
      2592000 ; Number of seconds in a month
      ))

(defun player-match-count (p)
  (+ (player-singles-won p)
     (player-singles-lost p)
     (player-doubles-won p)
     (player-doubles-lost p)))

(defun player-points-v1-diff (p &optional (span 2))
  (let ((history (player-points-v1-history p)))
    (if (>= (length history) span)
      (- (nth 0 history)
         (nth (- span 1) history))
      0)))

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

;;; - TEAM-MATE -----------------------------------------------------------------------------------

(defstruct team-mate
  name
  (won 0 :type fixnum)
  (lost 0 :type fixnum)
  (score-changes ()))

(defun team-mate-game-count (x)
  (+ (team-mate-won x)
     (team-mate-lost x)))

;;; - GAME ----------------------------------------------------------------------------------------

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

(defgeneric game-timestamp (game))

(defmethod game-timestamp ((game game-single))
  (game-single-timestamp game))

(defmethod game-timestamp ((game game-double))
  (game-double-timestamp game))

(defgeneric played-in-game-p (game player))

(defmethod played-in-game-p ((game game-single) player)
  (or (string= player (game-single-winner game))
      (string= player (game-single-looser game))))

(defmethod played-in-game-p ((game game-double) player)
  (or (string= player (game-double-winner-player-1 game))
      (string= player (game-double-winner-player-2 game))
      (string= player (game-double-looser-player-1 game))
      (string= player (game-double-looser-player-2 game))))
