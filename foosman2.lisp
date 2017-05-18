;;;; foosman2.lisp

(in-package #:foosman2)

(defun resource-path (path)
  "looks up path relative to whereever this asdf system is installed.  Returns a truename"
  (truename (asdf:system-relative-pathname :clift path)))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Set up server
;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *app* 
  (start (make-instance 'easy-acceptor :port 8777)))

(defvar *static* 
  (create-folder-dispatcher-and-handler "/static/" (resource-path "static")))

(defvar *ajax-processor*
  (make-instance 'ajax-processor :server-uri "/api"))

(setq *dispatch-table* (list 'dispatch-easy-handlers
                             *static*
                             (create-ajax-dispatcher *ajax-processor*)))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Serve HTML
;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-easy-handler (index :uri "/") ()
  (with-html-output-to-string (s)
    (:html
      (:head (:title "FoosMan2")
             (:link :rel "stylesheet" :href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css" 
                                      :integrity "sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u" 
                                      :crossorigin "anonymous")
             (:link :rel "stylesheet" :href "/static/bootstrap.superhero.min.css")
             (:link :rel "stylesheet" :href "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css")
             (:link :rel "stylesheet" :href "/static/foosman2.css"))
      (:body 
        (:nav :class "navbar navbar-default navbar-fixed-top"
          (:div :class "container-fluid"
            (:div :class "navbar-header"
              (:a :class "navbar-brand" :href "#" 
                (:i :class "fa fa-futbol-o" :aria-hidden "true")
                (str " FoosMan 2")))))
        (:div :class "container-fluid")
        (:script :src "https://code.jquery.com/jquery-3.2.1.min.js"
                 :integrity "sha256-hwg4gsxgFZhOsEEamdOYGBf13FyQuiTwlAQgxVSNgt4="
                 :crossorigin="anonymous")
        (:script :src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js" 
                 :integrity "sha384-Tc5IQib027qvyjSMfHjOMaLkfuWVxZxUPnCJA7l2mCWNIpG9mGCD8wGNIcPD7Txa" 
                 :crossorigin "anonymous")
        (str (generate-prologue *ajax-processor*))
        (:script :src "/static/foosman2.js")))))
