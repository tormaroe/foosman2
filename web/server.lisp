;;;; foosman2.lisp

(in-package #:foosman2-web.server)

(defun resource-path (path)
  "looks up path relative to whereever this asdf system is installed.  Returns a truename"
  (truename (asdf:system-relative-pathname :foosman2-web path)))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Set up server
;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(log:config :nopretty)

(initialize-event-processor)
(load-events)

(defvar *app* 
  (start (make-instance 'easy-acceptor :port 8777)))

(defvar *static* 
  (create-folder-dispatcher-and-handler "/static/" (resource-path "web/static")))

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

(defun json-array (xs)
  (format nil "[~{~a~^,~}]" xs))

(defun json-object (kvs)
  "Accepts list of field name/value pairs, pre-formatted"
  (->>
    kvs
    (mapcar (lambda (kv) 
              (format nil "~s:~a" (car kv) (cdr kv))))
    (format nil "{~{~a~^,~}}")))

(defun player-to-json (p)
  (json-object
    `(("name" . ,(format nil "~s" (player-name p)))
      ("singlesWon" . ,(player-singles-won p))
      ("singlesLost" . ,(player-singles-lost p))
      ("doublesWon" . ,(player-doubles-won p))
      ("doublesLost" . ,(player-doubles-lost p))
      ("pointsV1" . ,(player-points-v1 p))
      ("pointsV1Max" . ,(player-points-v1-max p))
      ("pointsV1History" . ,(json-array 
                              (player-points-v1-history p))))))

(defun-ajax all-players () (*ajax-processor* :callback-data :json)
  (->>
    (player-find-all)
    (mapcar #'player-to-json)
    (json-array)))

(defun-ajax get-player-details (name) (*ajax-processor* :callback-data :json)
  (log:info name)
  (player-to-json (player-by-name name)))

(defun-ajax new-player (name) (*ajax-processor* :callback-data :response-text)
  (log:info name)
  (command-add-player name)
  nil)

(defun-ajax new-game-single (data) (*ajax-processor* :callback-data :response-text)
  (log:info data)
  (let (winner looser)
    (dolist (x data)
      (case (car x)
        (:winner (setf winner (cdr x)))
        (:looser (setf looser (cdr x)))))
    (command-add-game-single winner looser))
  nil)

(defun-ajax new-game-double (data) (*ajax-processor* :callback-data :response-text)
  (log:info data)
  (let (winner1 winner2 looser1 looser2)
    (dolist (x data)
      (case (car x)
        (:winner-1 (setf winner1 (cdr x)))
        (:winner-2 (setf winner2 (cdr x)))
        (:looser-1 (setf looser1 (cdr x)))
        (:looser-2 (setf looser2 (cdr x)))))
    (command-add-game-double winner1 winner2 looser1 looser2))
  nil)

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

(defun new-game-single-form (s)
  (with-html-output (s)
    (:div :class "modal fade" :id "newGameSingleForm" :tabindex "-1" :role "dialog"
      (:div :class "modal-dialog" :role "document"
        (:div :class "modal-content"
          (:div :class "modal-header"
            (:h4 :class "modal-title" (str "New singles game")))
          (:div :class "modal-body"
            (:div :class "form-group"
              (:label :for "winnerName" (str "Winner"))
              (:select :id "winnerName" :v-model "newGameSingle.winner" :class "form-control"
                (:option :v-for "p in players"
                  (str "{{ p.name }}"))))
            (:div :class "form-group"
              (:label :for "looserName" (str "Looser"))
              (:select :id "looserName" :v-model "newGameSingle.looser" :class "form-control"
                (:option :v-for "p in players"
                  (str "{{ p.name }}")))))
          (:div :class "modal-footer"
            (:button :type "button" :class "btn btn-primary" :id "newGameSingleSave" :|v-on:click| "saveNewGameSingle"
              (str "Save"))))))))

(defun new-game-double-form (s)
  (with-html-output (s)
    (:div :class "modal fade" :id "newGameDoubleForm" :tabindex "-1" :role "dialog"
      (:div :class "modal-dialog" :role "document"
        (:div :class "modal-content"
          (:div :class "modal-header"
            (:h4 :class "modal-title" (str "New doubles game")))
          (:div :class "modal-body"
            (:div :class "form-group"
              (:label :for "winnerName1" (str "Winner 1"))
              (:select :id "winnerName1" :v-model "newGameDouble.winner1" :class "form-control"
                (:option :v-for "p in players"
                  (str "{{ p.name }}"))))
            (:div :class "form-group"
              (:label :for "winnerName2" (str "Winner 2"))
              (:select :id "winnerName2" :v-model "newGameDouble.winner2" :class "form-control"
                (:option :v-for "p in players"
                  (str "{{ p.name }}"))))
            (:div :class "form-group"
              (:label :for "looserName1" (str "Looser 1"))
              (:select :id "looserName1" :v-model "newGameDouble.looser1" :class "form-control"
                (:option :v-for "p in players"
                  (str "{{ p.name }}"))))
            (:div :class "form-group"
              (:label :for "looserName2" (str "Looser 2"))
              (:select :id "looserName2" :v-model "newGameDouble.looser2" :class "form-control"
                (:option :v-for "p in players"
                  (str "{{ p.name }}")))))
          (:div :class "modal-footer"
            (:button :type "button" :class "btn btn-primary" :id "newGameDoubleSave" :|v-on:click| "saveNewGameDouble"
              (str "Save"))))))))

(defun player-details (s)
  (with-html-output (s)
    (:div :class "row" :style "padding-bottom:15px;" :v-if "playerDetails"
      (:div :class "panel panel-default"
        (:div :class "panel-heading"
          (:button :class "btn btn-danger btn-xs pull-right" :|v-on:click| "closeDetails"
            (:i :class "fa fa-window-close-o" :aria-hidden "true"))
          (:h3 :class "panel-title" (str "{{ playerDetails.name }}")))
        (:div :class "panel-body"
          (:canvas :id "chartCanvas" :count "1" :height "80" :style "color:white;")
          (:chartjs-line
            :target "chartCanvas"
            ;:|:height| "80"
            ;:|:width| "600"
            :|:bind| "true"
            :|:options| "chartoption"
            :|:datalabel| "playerPointsV1Label"
            :|:labels| "playerPointsV1History"
            :|:data| "playerPointsV1History")
          (str "Player details coming here..."))))))

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
            (:div :class "container"
              (:div :class "navbar-header"
                (:a :class "navbar-brand" :href "#" 
                  (:i :class "fa fa-futbol-o" :aria-hidden "true")
                  (str " FoosMan 2")))))
          (new-player-form s)
          (new-game-single-form s)
          (new-game-double-form s)
          (:div :class "container"
            (player-details s)
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
                  (:th :style "text-align:center" (str "Points"))
                  (:th :style "text-align:center" (str "# Matches"))
                  (:th :style "text-align:center" :colspan "2" (str "Singles"))
                  (:th :style "text-align:center" :colspan "2" (str "Doubles"))
                  )
                (:tr :v-for "p in players"
                  (:td (:a :href "#" :|v-on:click| "displayPlayer(p.name)" (str "{{ p.name }}")))
                  (:td :style "text-align:center" (str "{{ p.pointsV1 }}"))
                  (:td :style "text-align:center" (str "{{ p.singlesWon + p.singlesLost + p.doublesWon + p.doublesLost }}"))
                  (:td :style "text-align:right" (str "{{ (p.singlesWon + p.singlesLost) > 0 ? Math.floor((p.singlesWon / (p.singlesWon + p.singlesLost)) * 100) : 0 }}%"))
                  (:td :style "text-align:left" (str "{{ p.singlesWon }} - {{ p.singlesLost }}"))
                  (:td :style "text-align:right" (str "{{ (p.doublesWon + p.doublesLost) > 0 ? Math.floor((p.doublesWon / (p.doublesWon + p.doublesLost)) * 100) : 0 }}%"))
                  (:td :style "text-align:left" (str "{{ p.doublesWon }} - {{ p.doublesLost }}"))
                  )))
            ) ; end container
          ) ; end vue app
        (:script :src "https://code.jquery.com/jquery-3.2.1.min.js"
                 :integrity "sha256-hwg4gsxgFZhOsEEamdOYGBf13FyQuiTwlAQgxVSNgt4="
                 :crossorigin="anonymous")
        (:script :src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js" 
                 :integrity "sha384-Tc5IQib027qvyjSMfHjOMaLkfuWVxZxUPnCJA7l2mCWNIpG9mGCD8wGNIcPD7Txa" 
                 :crossorigin "anonymous")
        (:script :src "https://unpkg.com/vue")
        (:script :src "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.3.0/Chart.js")
        (:script :src "/static/vue-charts.js")
        (str (generate-prologue *ajax-processor*))
        (:script :src "/static/foosman2.js")))))
