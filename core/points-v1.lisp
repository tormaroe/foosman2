
(in-package #:foosman2-core.points-v1)

(defparameter *initial-points* 1200)

(defun get-result-category (winner-points looser-points)
  (let ((difference (abs (- winner-points looser-points)))
        (underdog-won (< winner-points looser-points)))
    (if underdog-won
      (if (> difference 100)
        :huge-underdog-won
        :small-underdog-won)
      (if (> difference 100)
        :huge-favorite-won
        :small-favorite-won))))

(defun adjust-players (winner looser amount match-type)
  (set-points-v1 winner (+ (player-points-v1 winner) amount))
  (set-points-v1 looser (- (player-points-v1 looser) amount))
  (case match-type
    (:single 
      (incf (player-points-v1-singles winner) amount)
      (decf (player-points-v1-singles looser) amount))
    (:double 
      (incf (player-points-v1-doubles winner) amount)
      (decf (player-points-v1-doubles looser) amount))))

(defun get-team-points (p1 p2)
  (+ (max p1 p2)
     (let ((min-reduced (- (min p1 p2) 900)))
       (if (> min-reduced 0)
         (floor min-reduced 2)
         0))))

(defun adjust-single-game (winner looser)
  (incf (player-singles-won winner))
  (incf (player-singles-lost looser))
  (let ((point-amount-to-adjust 
         (case (get-result-category (player-points-v1 winner)
                                    (player-points-v1 looser))
           (:huge-favorite-won 0)
           (:small-favorite-won 5)
           (:huge-underdog-won 20)
           (:small-underdog-won 10))))
    (adjust-players winner looser point-amount-to-adjust :single)))

(defun adjust-double-game (winner-player-1 winner-player-2
                           looser-player-1 looser-player-2)
  (incf (player-doubles-won winner-player-1))
  (incf (player-doubles-won winner-player-2))
  (incf (player-doubles-lost looser-player-1))
  (incf (player-doubles-lost looser-player-2))
  (let* ((winner-team-points (get-team-points (player-points-v1 winner-player-1)
                                              (player-points-v1 winner-player-2)))
         (looser-team-points (get-team-points (player-points-v1 looser-player-1)
                                              (player-points-v1 looser-player-2)))
         (point-amount-to-adjust 
          (case (get-result-category winner-team-points
                                     looser-team-points)
            (:huge-favorite-won 0)
            (:small-favorite-won 3)
            (:huge-underdog-won 10)
            (:small-underdog-won 5))))
    (adjust-players winner-player-1 looser-player-1 point-amount-to-adjust :double)
    (adjust-players winner-player-2 looser-player-2 point-amount-to-adjust :double)))
