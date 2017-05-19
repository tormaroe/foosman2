;;;; model.lisp

(in-package #:foosman2.model)

(defstruct player
  name
  (singles-won 0 :type fixnum)
  (singles-lost 0 :type fixnum)
  (doubles-won 0 :type fixnum)
  (doubles-lost 0 :type fixnum)
  (points-v1 0 :type fixnum))