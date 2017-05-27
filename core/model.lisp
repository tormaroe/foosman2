;;;; model.lisp

(in-package #:foosman2-core.model)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   MAGIC
;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defmodel (name &rest spec)
  (labels ((join-symbols (a b)
             (intern (string-upcase
                       (format nil "~a-~a" a b))))
           (symbol-to-keyword (s)
             (intern (string s) "KEYWORD")))
    (let* ((js-names (mapcar #'car spec))
           (struct-slots (mapcar #'cdr spec))
           (slot-names (mapcar (lambda (x) (if (listp x) (car x) x)) 
                               struct-slots))
           (to-json-format (format nil "{~{~a~^,~}}"
                                   (mapcar (lambda (x) (format nil "~s:~~s" x))
                                           js-names)))
           (slot-accessors (mapcar (lambda (x) (join-symbols name x))
                                   slot-names))
           (to-json-parameter (gensym)))
      `(progn
         ;; DECLARE STRUCT xxx
         (defstruct ,name
           ,@struct-slots)
         ;; DECLARE xxx-to-json
         (defun ,(join-symbols name 'to-json) (,to-json-parameter)
           (format nil ,to-json-format 
                   ,@(mapcar (lambda (x) (list x to-json-parameter)) ; TODO: Fix NIL
                             slot-accessors)))
         ))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   MODELS
;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodel player
  ("name" . name)
  ("singlesLost" . (singles-won 0 :type fixnum))
  ("singlesWon" . (singles-lost 0 :type fixnum))
  ("doublesLost" . (doubles-won 0 :type fixnum))
  ("doublesWon" . (doubles-lost 0 :type fixnum))
  ("pointsV1" . (points-v1 0 :type fixnum)))

