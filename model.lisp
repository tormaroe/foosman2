;;;; model.lisp

(in-package #:foosman2.model)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   MAGIC
;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(import 'cl-mongo::make-bson-oid)

(defun oid->string (oid)
  ;(subseq 
    (string-downcase
      (cl-ppcre:regex-replace-all "[#( )]" (format nil "~X" oid) ""))
    ;0 22)
  )

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
           (collection (join-symbols name 'collection))
           (to-json-parameter (gensym))
           (save-parameter (gensym))
           (save-doc (gensym))
           (from-doc-parameter (gensym))
           (make-player-args (apply #'append
                               (mapcar (lambda (slot-symbol js-name)
                                         (list (symbol-to-keyword slot-symbol)
                                               (list 'cl-mongo:get-element 
                                                     js-name 
                                                     from-doc-parameter)))
                                       slot-names
                                       js-names))))
      `(progn
         ;; DECLARE STRUCT xxx
         (defstruct ,name
           ,@struct-slots
           model-id)
         ;; DECLARE xxx-to-json
         (defun ,(join-symbols name 'to-json) (,to-json-parameter)
           (format nil ,to-json-format 
                   ,@(mapcar (lambda (x) (list x to-json-parameter)) ; TODO: Fix NIL
                             slot-accessors)))
         ;; DECLARE xxx-collection
         (defvar ,collection
                 ,(string-downcase (string name)))
         ;; DECLARE save-xxx
         (defun ,(join-symbols 'save name) (,save-parameter)
           (let ((,save-doc (cl-mongo:make-document))); :oid (cl-mongo::make-bson-oid :oid (mongoid:oid)))))
             ,@(mapcar (lambda (js-name slot-accessor)
                         (list 'cl-mongo:add-element
                               js-name 
                               (list slot-accessor save-parameter)
                               save-doc))
                       js-names
                       slot-accessors)
             (cl-mongo:db.insert ,collection ,save-doc)))
         ;; DECLARE xxx-from-doc
         (defun ,(join-symbols name 'from-doc) (,from-doc-parameter)
           (,(join-symbols 'make name) ,@make-player-args
              :model-id (oid->string (cl-mongo:doc-id ,from-doc-parameter))))
         ;; DECLARE xxx-find-all
         (defun ,(join-symbols name 'find-all) ()
           (mapcar (quote ,(join-symbols name 'from-doc))
                   (cl-mongo:docs (cl-mongo:db.find ,collection :all))))
         ))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   MODELS
;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodel player
  ;("_id" . id)
  ("name" . name)
  ("singlesLost" . (singles-won 0 :type fixnum))
  ("singlesWon" . (singles-lost 0 :type fixnum))
  ("doublesLost" . (doubles-won 0 :type fixnum))
  ("doublesWon" . (doubles-lost 0 :type fixnum))
  ("pointsV1" . (points-v1 0 :type fixnum)))

