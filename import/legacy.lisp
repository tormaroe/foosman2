
(defpackage #:foosman2-import.legacy
  (:use #:cl 
        #:alexandria
        #:cl-arrows
        #:foosman2-core.data))

(in-package #:foosman2-import.legacy)

(defparameter *players-filepath* "C:/dev/foosman2/data/players.xml")
(defparameter *audittrail-filepath* "C:/dev/foosman2/data/audittrail.xml")
(setf foosman2-core.data::*event-log-pathname* "c:/temp/foosball2.test.events")

(defun parse-xml (filepath)
  (with-input-from-file (stream filepath :external-format :UTF-8)
    (xmls:parse stream)))

(defun load-players ()
  (->>
    *players-filepath*
    (parse-xml)
    (cddr) ; Drop root element and attributes
    (mapcar #'cddr) ; Drop player root nodes and attributes
    (mapcar (lambda (attributes) 
              (remove-if-not (lambda (node)
                               (string= "name" (car node)))
                             attributes)))
    (mapcar #'caddar) ;; Extract the name list

    (mapcar (lambda (name)
              (foosman2-core.data::make-event-player-added
                :player-name name
                :id (incf foosman2-core.data::*current-event-id*))))

    (mapcar #'foosman2-core.data::save-event-to-file))
  :done)

(defun convert-legacy-timestamp (legacy)
  "Converts from Unix time (seconds since 1970.01.01T00:00)
   formatted as a float string with #\, as decimal separator 
   to universal time integer (seconds since 1900.01.01T00:00)
   Example: \"1494932161,508\" => 3703920961"
   ;; Lisp epoc vs. Unix epoc diff: 2208988800 seconds
  (->> legacy
    (read-from-string)
    (+ 2208988800)))

(defun load-audittrail ()
  (->>
    *audittrail-filepath*
    (parse-xml)
    (cddr) ; Drop root element and attributes
    (mapcar #'cddr) ; Drop item root nodes and attributes

    (mapcar ; make pair: (when . what) 
      (lambda (attributes)
        (cons (convert-legacy-timestamp
                (caddr (find "when" attributes :key #'car :test #'string=)))
              (caddr (find "what" attributes :key #'car :test #'string=)))))

    (remove-if 
      (lambda (x) 
        (cl-ppcre:scan "'s score changed to \\d+ \\(\\d+ points was" 
                       (cdr x))))

    (mapcar
      (lambda (x)
        (multiple-value-bind (m ms)
            (cl-ppcre:scan-to-strings "^(.+) won a singles match agains (.+)\\.$" (cdr x))
          (if m 
            (foosman2-core.data::make-event-single-game-added
              :id (incf foosman2-core.data::*current-event-id*)
              :game (foosman2-core.model::make-game-single :winner (elt ms 0)
                                                           :looser (elt ms 1)
                                                           :timestamp (car x)))
            (multiple-value-bind (m ms)
                (cl-ppcre:scan-to-strings "^(.+) and (.+) won a doubles match agains (.+) and (.+)\\.$" (cdr x))
              (if m 
                (foosman2-core.data::make-event-double-game-added
                  :id (incf foosman2-core.data::*current-event-id*)
                  :game (foosman2-core.model::make-game-double :winner-player-1 (elt ms 0)
                                                               :winner-player-2 (elt ms 1)
                                                               :looser-player-1 (elt ms 2)
                                                               :looser-player-2 (elt ms 3)
                                                               :timestamp (car x)))
                (multiple-value-bind (m ms)
                    (cl-ppcre:scan-to-strings "^Manual adjustment of player (.+): SW: \\d+->(\\d+), SL: \\d+->(\\d+), DW: \\d+->(\\d+), DL: \\d+->(\\d+), Points: \\d+->(\\d+)$" (cdr x))
                  (if m 
                    (foosman2-core.data::make-event-manual-adjustment
                      :id (incf foosman2-core.data::*current-event-id*)
                      :timestamp (car x)
                      :player (elt ms 0)
                      :singles-won (parse-integer (elt ms 1))
                      :singles-lost (parse-integer (elt ms 2))
                      :doubles-won (parse-integer (elt ms 3))
                      :doubles-lost (parse-integer (elt ms 4))
                      :points-v1 (parse-integer (elt ms 5)))
                    (error "UNKNOWN EVENT")))))))))

    (mapcar #'foosman2-core.data::save-event-to-file))
  :done)

