;;;; foosman2.lisp

(in-package #:foosman2)

(defun resource-path (path)
  "looks up path relative to whereever this asdf system is installed.  Returns a truename"
  (truename (asdf:system-relative-pathname :foosman2 path)))


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

(setf *js-string-delimiter* #\")


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   API
;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun-ajax all-players () (*ajax-processor* :callback-data :json)
  ;; TODO: make generic json transformation
  (format nil "[ ~{ ~a ~^, ~} ]"
    (mapcar 
      (lambda (p)
        (format nil "{
                       \"name\": ~s,
                       \"singlesLost\": ~s,
                       \"singlesWon\": ~s,
                       \"doublesLost\": ~s,
                       \"doublesWon\": ~s,
                       \"pointsV1\": ~s
                     }" 
                     (player-name p)
                     (player-singles-lost p)
                     (player-singles-won p)
                     (player-doubles-lost p)
                     (player-doubles-won p)
                     (player-points-v1 p))) 
      (get-players))))

(defun-ajax new-player (name) (*ajax-processor* :callback-data :response-text)
  (format t "~&API: new-player ~s~%" name)
  (save-player (make-player :name name)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Serve HTML
;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun new-player-form (s)
  (with-html-output (s)
    (:div :class "modal fade" :id "newPlayerForm" :tabindex "-1" :role "dialog"
      (:div :class "modal-dialog" :role "document"
        (:div :class "modal-content"
          (:div :class "modal-header"
            (:h4 :class "modal-title" (str "New player")))
          (:div :class "modal-body"
            (:input :id "newPlayerName" :class "form-control" :v-model "newPlayerName"))
          (:div :class "modal-footer"
            (:button :type "button" :class "btn btn-primary" :id "newPlayerSave" :|v-on:click| "saveNewPlayer"
              (str "Save"))))))))

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
        (:div :id "foosman2App"
          (:nav :class "navbar navbar-default navbar-fixed-top"
            (:div :class "container-fluid"
              (:div :class "navbar-header"
                (:a :class "navbar-brand" :href "#" 
                  (:i :class "fa fa-futbol-o" :aria-hidden "true")
                  (str " FoosMan 2")))))
          (new-player-form s)
          (:div :class "container"
            (:div :class "row" :style "padding-bottom:15px;"
              (:div :class "well"
                (str "Chart placeholder")))
            (:div :class "row" :style "padding-bottom:15px;"
              (:button :class "btn btn-primary" :|v-on:click| "initiateNewSingleGame"
                (str "Add match (single)"))
              (:button :class "btn btn-primary" :|v-on:click| "initiateNewDoubleGame"
                (str "Add match (double)"))
              (:button :class "btn btn-default" :|v-on:click| "initiateNewPlayer"
                (str "Add new player")))
            (:div :class "row"
              (:table :class "table table-striped"
                (:tr 
                  (:th (str "Player"))
                  (:th :style "text-align:right" (str "Singles Won"))
                  (:th :style "text-align:right" (str "Singles Lost"))
                  (:th :style "text-align:right" (str "Doubles won"))
                  (:th :style "text-align:right" (str "Doubles lost"))
                  (:th :style "text-align:right" (str "Points")))
                (:tr :v-for "p in players"
                  (:td (str "{{ p.name }}"))
                  (:td :style "text-align:right" (str "{{ p.singlesWon }}"))
                  (:td :style "text-align:right" (str "{{ p.singlesLost }}"))
                  (:td :style "text-align:right" (str "{{ p.doublesWon }}"))
                  (:td :style "text-align:right" (str "{{ p.doublesLost }}"))
                  (:td :style "text-align:right" (str "{{ p.pointsV1 }}")))))
            ) ; end container
          ) ; end vue app
        (:script :src "https://code.jquery.com/jquery-3.2.1.min.js"
                 :integrity "sha256-hwg4gsxgFZhOsEEamdOYGBf13FyQuiTwlAQgxVSNgt4="
                 :crossorigin="anonymous")
        (:script :src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js" 
                 :integrity "sha384-Tc5IQib027qvyjSMfHjOMaLkfuWVxZxUPnCJA7l2mCWNIpG9mGCD8wGNIcPD7Txa" 
                 :crossorigin "anonymous")
        (:script :src "https://unpkg.com/vue")
        (str (generate-prologue *ajax-processor*))
        (:script :src "/static/foosman2.js")))))
