;;;; data.lisp

(in-package #:foosman2-core.data)


;;; Persistance mechanics:
;;; A thread consumes from a channel which acts as a serialized even stream
;;; It persists the events by appending to a file
;;;
;;; At intervals a snapshot will be made and persisted, and the event queue will be cleared?
;;; Or give each event a unique id (sequential), and when you load a snapshot skip over included events

;;; To re-play events, use multimethods polimorphism

;;; -----------------------------------------------------------------------------------------------
;;; DATA STATE VARIABLES
;;; State is *only* mutated by handle-event methods, running in the context of a single thread.
;;; -----------------------------------------------------------------------------------------------

(defparameter *players* ())
(defparameter *games* ())

;;; -----------------------------------------------------------------------------------------------
;;; DATA QUERIES
;;; -----------------------------------------------------------------------------------------------

(defun player-find-all ()
  *players*)

(defun player-by-name (name)
  (find name
        *players*
        :key #'player-name
        :test #'string=))

(defun games-by-player (name)
  (remove-if (lambda (g)
               (not (played-in-game-p g name)))
             *games*))

;;; -----------------------------------------------------------------------------------------------
;;; DATA EVENT DEFINITIONS AND PROCESSING
;;; -----------------------------------------------------------------------------------------------

(defparameter *event-channel* nil)

(defgeneric handle-event (event))

;; TODO: Each player keeps track of each other player and their relative "score". Display best team mate, nemesis, etc.
;; TODO: validate new place name not blank (client side)
;; TODO: validate unique players selected when registering match (client side)
;; TODO: Failure flash client side


(defstruct event-player-added id player-name)

(defmethod handle-event ((event event-player-added))
  (-> (make-player :name (event-player-added-player-name event)
                   :points-v1-min foosman2-core.points-v1:*initial-points*
                   :points-v1-singles foosman2-core.points-v1:*initial-points*
                   :points-v1-doubles foosman2-core.points-v1:*initial-points*)
      (set-points-v1 foosman2-core.points-v1:*initial-points*)
      (push *players*)))

(defun command-add-player (name)
  (log:info name)
  (if (notany (lambda (p) (string= name (player-name p))) *players*)
    (send *event-channel* 
          (make-event-player-added :player-name name))
    (log:warn "Name not unique. TODO: Add UX feedback.")))


(defstruct event-single-game-added id game)

(defmethod handle-event ((event event-single-game-added))
  (let* ((game (event-single-game-added-game event))
         (winner (player-by-name (game-single-winner game)))
         (looser (player-by-name (game-single-looser game))))
    (foosman2-core.points-v1:adjust-single-game winner looser)
    (award-badges *players* (game-single-timestamp game))
    (push game *games*)))

(defun command-add-game-single (winner-name looser-name)
  (log:info winner-name looser-name)
  (let ((winner (player-by-name winner-name))
        (looser (player-by-name looser-name)))
    (if (and winner looser)
      (send *event-channel* 
            (make-event-single-game-added 
              :game (make-game-single :winner winner-name
                                      :looser looser-name
                                      :timestamp (get-universal-time))))
      (log:warn "Can't find one of the players. TODO: Add UX feedback?"))))

(defstruct event-double-game-added id game)

(defmethod handle-event ((event event-double-game-added))
  (let* ((game (event-double-game-added-game event))
         (winner1 (player-by-name (game-double-winner-player-1 game)))
         (winner2 (player-by-name (game-double-winner-player-2 game)))
         (looser1 (player-by-name (game-double-looser-player-1 game)))
         (looser2 (player-by-name (game-double-looser-player-2 game))))
    (foosman2-core.points-v1:adjust-double-game winner1 winner2 looser1 looser2)
    (award-badges *players* (game-double-timestamp game))))

(defun command-add-game-double (winner-name-1 winner-name-2 
                                looser-name-1 looser-name-2)
  (log:info winner-name-1 winner-name-2 looser-name-1 looser-name-2)
  (let ((winner1 (player-by-name winner-name-1))
        (winner2 (player-by-name winner-name-2))
        (looser1 (player-by-name looser-name-1))
        (looser2 (player-by-name looser-name-2)))
    (if (and winner1 winner2 looser1 looser2)
      (send *event-channel* 
            (make-event-double-game-added 
              :game (make-game-double :winner-player-1 winner-name-1
                                      :winner-player-2 winner-name-2
                                      :looser-player-1 looser-name-1
                                      :looser-player-2 looser-name-2
                                      :timestamp (get-universal-time))))
      (log:warn "Can't find one of the players. TODO: Add UX feedback?"))))

(defstruct event-manual-adjustment
  id
  timestamp
  player
  singles-won
  singles-lost
  doubles-won
  doubles-lost
  points-v1)

(defmethod handle-event ((event event-manual-adjustment))
  (let ((p (player-by-name (event-manual-adjustment-player event))))
    (setf (player-singles-won p)
          (event-manual-adjustment-singles-won event))
    (setf (player-singles-lost p)
          (event-manual-adjustment-singles-lost event))
    (setf (player-doubles-won p)
          (event-manual-adjustment-doubles-won event))
    (setf (player-doubles-lost p)
          (event-manual-adjustment-doubles-lost event))
    (setf (player-points-v1 p)
          (event-manual-adjustment-points-v1 event))))

;;; -----------------------------------------------------------------------------------------------
;;; EVENT MESSAGE PUMP
;;; -----------------------------------------------------------------------------------------------

(defparameter *current-event-id* 0) ; Will increment before using

(defparameter *event-log-pathname* "c:/temp/foosball2.events")

(defun serialize (struct)
  (-<>> struct
    (format nil "~s")
    (cl-ppcre:regex-replace-all "\\s\\s+" <> " ")
    (substitute #\SPACE #\NEWLINE)
    (format nil "~a~%")))

(defun save-event-to-file (e)
  (write-string-into-file (serialize e)
                          *event-log-pathname*
                          :if-does-not-exist :create
                          :if-exists :append
                          :external-format :UTF-8))

(defun consume-events ()
  (let ((e (recv *event-channel*)))
    (log:info "Received event" e)
    (handle-event e)
    (unless (slot-value e 'id)
      (setf (slot-value e 'id) 
            (incf *current-event-id*))
      (save-event-to-file e))
  (consume-events)))

(defun initialize-event-processor ()
  (log:info "Setting up event channel and consumer")
  (setf *event-channel* (make-instance 'channel))
  (pcall #'consume-events)
  (log:info "Setting up event channel and consumer done"))

(defun load-events ()
  (log:info "Loading events stored at" *event-log-pathname*)
  (setf *players* ())
  (setf *games-single* ())
  (setf *games-double* ())
  (with-input-from-file (stream *event-log-pathname* :external-format :UTF-8)
    (loop for event = (read stream nil)
          while event 
          do (progn
               (send *event-channel* event)
               (setf *current-event-id*
                     (slot-value event 'id)))))
  (log:info "Loading events done." *current-event-id*))

