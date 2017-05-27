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
;;; -----------------------------------------------------------------------------------------------

(defparameter *users* ())

;;; -----------------------------------------------------------------------------------------------
;;; DATA EVENT DEFINITIONS AND PROCESSING
;;; -----------------------------------------------------------------------------------------------

(defgeneric handle-event (event))

(defstruct event-add-player id player)

(defmethod handle-event ((event event-add-player))
  (push (event-add-player-player event)
        *users*))

;;; -----------------------------------------------------------------------------------------------
;;; EVENT MESSAGE PUMP
;;; -----------------------------------------------------------------------------------------------

(defparameter *current-event-id* 0) ; Will increment before using

(defparameter *event-channel* nil)

(defparameter *event-log-pathname* "c:/temp/foosball2.events")

(defun serialize (struct)
  (-<>> struct
    (format nil "~s")
    (cl-ppcre:regex-replace-all "\\s\\s+" <> " ")
    (substitute #\SPACE #\NEWLINE)
    (format nil "~a~%")))

(defun consume-events ()
  (let ((e (recv *event-channel*)))
    (log:info "Received event" e)
    (handle-event e)
    (when (null (slot-value e 'id))
      (setf (slot-value e 'id) 
            (incf *current-event-id*))
      (write-string-into-file (serialize e)
                              *event-log-pathname*
                              :if-does-not-exist :create
                              :if-exists :append
                              :external-format :UTF-8)))
  (consume-events))

(defun initialize-event-processor ()
  (log:info "Setting up event channel and consumer")
  (setf *event-channel* (make-instance 'channel))
  (pcall #'consume-events)
  (log:info "Setting up event channel and consumer done"))

;;; -----------------------------------------------------------------------------------------------
;;; DATA COMMANDS
;;; -----------------------------------------------------------------------------------------------

(defun command-add-player (name)
  (log:info name)
  (if (notany (lambda (p) (string= name (player-name p))) *users*)
    (send *event-channel* 
          (make-event-add-player :player (make-player :name name))))
    (log:warn "Name not unique. TODO: Add UX feedback."))

;;; -----------------------------------------------------------------------------------------------
;;; DATA QUERIES
;;; -----------------------------------------------------------------------------------------------

(defun player-find-all ()
  (log:info *users*)
  *users*)