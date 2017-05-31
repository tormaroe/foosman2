
(in-package #:foosman2-core.badges)

;; TODO: Badges: When adjusting points, do a pass and award badges. Badges are never lost, but each badge is awarded only once.
;;       Badge has ICON, DATE, DESCRIPTION. Examples:
;;  - "Individualist": Highest score singles (minimum X single matches)
;;  - "Team Player": Highest score doubles (minimum X double matches)
;;  - "Winning Streak"
;;  -

(defstruct badge
  key
  title
  class
  description)

(defun format-timestamp (ts)
  (multiple-value-bind
      (second minute hour date month year day-of-week dst-p tz)
      (decode-universal-time ts)
    (format nil "~2,'0d.~2,'0d.~d ~2,'0d:~2,'0d"
            date
            month
            year
            hour
            minute)))

(defun get-best (players test)
  (labels ((f (current rest)
             (if rest
               (let ((next (car rest)))
                 (if (funcall test current next)
                   (f current (cdr rest))
                   (f next (cdr rest))))
               current)))
    (f (car players) (cdr players))))

(defun award-badge (players &key key title class test make-description)
  (let ((active-players (remove-if (lambda (x) (> 10 (player-match-count x))) players)))
    (when active-players
      (let ((best (get-best active-players test)))
        (unless (find key 
                      (player-badges best)
                      :key #'badge-key)
          (push (make-badge :key key
                            :title title
                            :description (funcall make-description best)
                            :class class) 
                (player-badges best)))))))

(defun award-numero-uno-badge (players timestamp)
  (award-badge players 
               :key :numero-uno
               :title "Numero Uno!"
               :class "fa fa-trophy"
               :test (lambda (a b)
                       (> (player-points-v1 a)
                          (player-points-v1 b)))
               :make-description
                 (lambda (best)
                   (format nil "~a ~a achieved the highest score of all active players: ~a"
                           (format-timestamp timestamp)
                           (player-name best)
                           (player-points-v1 best)))))

(defun award-solid-badge (players timestamp)
  (award-badge players 
               :key :solid
               :title "Solid!"
               :class "fa fa-shield"
               :test (lambda (a b)
                       (> (player-points-v1-average a)
                          (player-points-v1-average b)))
               :make-description
                 (lambda (best)
                   (format nil "~a ~a achieved the highest average score of all active players: ~a"
                           (format-timestamp timestamp)
                           (player-name best)
                           (player-points-v1-average best)))))

(defun award-eager-beaver-badge (players timestamp)
  (award-badge players 
               :key :eager-beaver
               :title "Eager Beaver!"
               :class "fa fa-fighter-jet"
               :test (lambda (a b)
                       (> (player-match-count a)
                          (player-match-count b)))
               :make-description
                 (lambda (best)
                   (format nil "~a ~a had played the highest number of matches: ~a"
                           (format-timestamp timestamp)
                           (player-name best)
                           (player-match-count best)))))

(defun award-badges (players timestamp)
	(award-numero-uno-badge players timestamp)
  (award-solid-badge players timestamp)
  (award-eager-beaver-badge players timestamp))