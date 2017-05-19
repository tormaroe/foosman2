;;;; data.lisp

(in-package #:foosman2.data)

(defvar +collection-player+ "player")

(defun save-player (p)
  (let ((doc (make-document)))
    (add-element "name" (player-name p) doc)
    (add-element "singlesLost" (player-singles-lost p) doc)
    (add-element "singlesWon" (player-singles-won p) doc)
    (add-element "doublesLost" (player-doubles-won p) doc)
    (add-element "doublesWon" (player-doubles-lost p) doc)
    (add-element "pointsV1" (player-points-v1 p) doc)
    (db.insert +collection-player+ doc)))

(defun get-players ()
  (flet ((doc->player (doc)
            (make-player 
              :name (get-element "name" doc)
              :singles-won (get-element "singlesWon" doc)
              :singles-lost (get-element "singlesLost" doc)
              :doubles-won (get-element "doublesWon" doc)
              :doubles-lost (get-element "doublesLost" doc))))
    (mapcar #'doc->player 
            (docs (db.find +collection-player+ :all)))))