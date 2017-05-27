;;;; data.lisp

(in-package #:foosman2-core.data)

(defparameter *users* ())

(defun save-player (p)
  (push p *users*)
  (format t "save-player ~a~%" p)
)

(defun player-find-all ()
  *users*)