
#lang racket

(require net/http-easy)
(require text-table)
(require json/format/simple)
(require json)
(require (prefix-in s. srfi/19))

(define base-url "https://notenmanagement.htl-braunau.at/rest")

(define (rfc2822->unix-time s)
  ;; Replace "GMT" with "+0000" to match the expected format by string->date
  (let* ((adjusted-s (string-replace s "GMT" "+0000"))
         (date (s.string->date adjusted-s "~a, ~d ~b ~Y ~H:~M:~S ~z")))
    (s.time-second (s.date->time-utc date))))

(define (string-replace str find replace)
  (regexp-replace* (regexp-quote find) str replace))

(define access-token (make-parameter #f))
(define session (make-parameter #f))
(define output-format (make-parameter "table"))
(define fach (make-parameter #f))
(define action (make-parameter #f))
(define matrNr (make-parameter #f))
(define klasse (make-parameter #f))
(define lf-id (make-parameter #f))
(define mandatoryarg (make-parameter #f))

(define (res-err operation status [custom-msg #f])
  (let* ([default-msgs (hash '401 "Zugriff verweigert"
                             '404 "Ressource nicht gefunden"
                             '500 "Internal Server Error bei Notenmanagement")]
         [msg (if custom-msg custom-msg (hash-ref default-msgs status "Unknown Error"))])
    (error (string-append "Fehler bei " operation ": " msg " (Status: " (number->string status) ")"))))

(define (make-authenticated-request endpoint [params #f])
  (unless (access-token)
    (error "No access token available."))
  (let* ([auth-header (bearer-auth (access-token))]
         [url (string-append base-url endpoint)]
         [response (if params
                       (post url #:auth auth-header #:form params)
                       (get url #:auth auth-header #:headers '#hash((Content-Type . "application/x-www-form-urlencoded"))))]
         [status (response-status-code response)])
    (case status
      [(200) (response-json response)]
      [else (res-err endpoint status)])))



(define (getGradesForStudent matrNr  [subject #f])
  (cond
    [subject
     (make-authenticated-request
      (string-append "/api" "/Schueler/" (number->string matrNr) "/Faecher/" subject "/Noten"))]
    [else
     (make-authenticated-request
      (string-append "/api" "/Schueler/" (number->string matrNr) "/Noten?sort=Fach"))]))

(define (getLFs klasse [subject #f])
  (let ([url (cond
               [(and klasse subject)
                (string-append "/api/Klassen/" klasse "/Faecher/" subject "/LFs")]
               [klasse
                (string-append "/api/Klassen/" klasse "/LFs?sort=Fach")]
               [else
                (error "Invalid parameters provided")])])
    (make-authenticated-request url)))

(define (getGradesForLF lfId  [matrNr #f] )
  (cond
    [matrNr
     (make-authenticated-request
      (string-append "/api" "/LFs/" (number->string lfId) "/Schueler/" (number->string matrNr) "/Noten"))]
    [else
     (make-authenticated-request
      (string-append "/api" "/LFs/" (number->string lfId) "/Noten"))]))


(define (getSubjects [matrNr #f])
  (let ([url (if matrNr
                 (string-append "/api" "/Schueler/" (number->string matrNr) "/Faecher")
                 (string-append "/api" "/Faecher"))])
    (make-authenticated-request url )))

(define (getAbsences matrNr)
  (make-authenticated-request (format "/api/Schueler/~a/Fehlstunden" (number->string matrNr))))

(define (getStudent [matrNr #f] [klasse #f])
  (let ([url (cond
               [matrNr (string-append "/api/Schueler/" (number->string matrNr))]
               [klasse (format "/api/Klassen/~a/Schueler" klasse)]
               [else "/api/Schueler"])]) ; Default or fallback URL
    (make-authenticated-request url)))

(define (getFW [matrNr #f] [klasse #f] [subject #f])
  (let ([url (cond
               [(and matrNr subject)
                (string-append "/api/Schueler/" (number->string matrNr) "/Faecher/" subject "/Fruehwarnungen")]
               [matrNr
                (string-append "/api/Schueler/" (number->string matrNr) "/Fruehwarnungen")]
               [(and klasse subject)
                (string-append "/api/Klassen/" klasse "/Faecher/" subject "/Fruehwarnungen")]
               [klasse
                (string-append "/api/Klassen/" klasse "/Fruehwarnungen")]
               [else (error "Invalid combination of parameters")])])
    (make-authenticated-request url)))


(define (generateStudentInfo matrNr)
  (define (map-assessments grades subject)
    (filter-map (lambda (grade)
                  (if (equal? (hash-ref grade 'Fach) (hash-ref subject 'Fach))
                      (make-hasheq (list
                                    (cons 'id (hash-ref grade 'LF_ID))
                                    (cons 'date (hash-ref grade 'Datum))
                                    (cons 'type (hash-ref grade 'Typ))
                                    (cons 'comment (hash-ref grade 'LF_Kommentar))
                                    (cons 'maxPoints (hash-ref grade 'MaxPunkte))
                                    (cons 'teacherId (hash-ref grade 'Lehrer_ID))
                                    (cons 'grade (make-hasheq (list
                                                               (cons 'grade (hash-ref grade 'Note))
                                                               (cons 'points (hash-ref grade 'Punkte))
                                                               (cons 'comment (hash-ref grade 'Kommentar)))))))
                      #f))
                grades))
  (let* ([studentInfo (getStudent matrNr)]
         [grades (getGradesForStudent matrNr)]
         [absences (getAbsences matrNr)]
         [subjects (getSubjects matrNr)]
         [json-obj (make-hasheq (list (cons 'matNo (hash-ref studentInfo 'Matrikelnummer))
                                      (cons 'firstname (hash-ref studentInfo 'Vorname))
                                      (cons 'lastname (hash-ref studentInfo 'Nachname)) ; Changed to symbol
                                      (cons 'className (hash-ref studentInfo 'Klasse))
                                      (cons 'email1 (hash-ref studentInfo 'EMailAdresse1))
                                      (cons 'email2 (if (hash-ref studentInfo 'EMailAdresse2) ; Changed to symbol
                                                        (hash-ref studentInfo 'EMailAdresse2)
                                                        null))
                                      (cons 'absences (make-hasheq (list (cons 'excused (hash-ref absences 'Fehlstunden_Entschuldigt)) ; Changed to symbol
                                                                         (cons 'unexcused (hash-ref absences 'Fehlstunden_NichtEntschuldigt)) ; Changed to symbol
                                                                         (cons 'open (hash-ref absences 'Fehlstunden_Offen))))) ; Changed to symbol
                                      (cons 'subjects (map (lambda (subject)
                                                             (make-hasheq (list (cons 'id (hash-ref subject 'Fach_ID)) ; Changed to symbol
                                                                                (cons 'name (hash-ref subject 'Fach)) ; Changed to symbol
                                                                                (cons 'description (hash-ref subject 'Fachbezeichnung))
                                                                                (cons 'assessments (map-assessments grades subject))
                                                                                ))) ; Changed to symbol
                                                           subjects))))])
    json-obj))




(define (hashes-to-lists hashes)
  ;; Check if the input is not a list, meaning it's a single hash, and wrap it in a list
  (let ([hashes-list (if (list? hashes) hashes (list hashes))])

    (let* ((keys (hash-keys (first hashes-list)))
           (rows (map (lambda (hash)
                        (map (lambda (key) (hash-ref hash key))
                             keys))
                      hashes-list)))
      (cons keys rows))))

(define (print-data data)
  (match data
    [(list)
     (printf "No data available.\n")]
    [_
     (cond
       [(equal? (output-format) "json")
        (pretty-print-jsexpr data)]
       [(equal? (output-format) "ppable")
        (print-table (hashes-to-lists data))]
       [else
        (print-simple-table (hashes-to-lists data))])]))

(define (write-hash-to-json-file hash-table file-path)
  (call-with-output-file file-path
    (lambda (out)
      (write-json hash-table out))
    #:exists 'replace))

(define (read-hash-from-json-file file-path)
  (if (file-exists? file-path)
      (call-with-input-file file-path
        (lambda (in)
          (read-json in)))
      (begin
        (displayln "Error: Not logged in - session file not found.")
        #f))) ; Returning #f or a similar value to indicate the absence of valid data

(define (make-login-request username password)
  (let* ([params `((grant_type . "password")
                   (username . ,username)
                   (password . ,password))]
         [url (string-append base-url "/token")] ; Assume base-url is defined elsewhere
         [response (post url #:form params)]
         [status (response-status-code response)]
         [operation "Login Request: "])
    (if (= status 200)
        (response-json response)
        (res-err operation status (if (= status 400) "Ungültige Anmeldedaten" #f)))))

(define (perform-login username password)
  (define loginres (make-login-request username password))
  (access-token (hash-ref loginres 'access_token))
  (session loginres)
  (write-hash-to-json-file loginres "/tmp/nmcli-session.json")
  (printf "~a\n" (string-append "logged in as: " (hash-ref (session) 'role)
                                (if (hash-has-key? (session) 'matrikelNr)
                                    (string-append " matrikelNr: " (hash-ref (session) 'matrikelNr))
                                    "")))
  )

(define (choose-action action)
  (case action
    [("subjects") (print-data(getSubjects (matrNr)))]
    [("absences") (print-data(getAbsences (matrNr)))]
    [("lf") (print-data (getLFs (klasse) (fach)))]
    [("grades") (print-data (getGradesForStudent (matrNr) (fach)))]
    [("lf-grades") (print-data (getGradesForLF (lf-id) (matrNr)))]
    [("fw") (print-data (getFW (matrNr) (klasse) (fach)))]
    [("studentinfo") (print-data (getStudent (matrNr) (klasse)))]
    [("dump") (pretty-print-jsexpr (generateStudentInfo (matrNr))) ]
    [else 'unknown]))

;; command line parser
(define parser
  (command-line
   #:usage-help
   "CLI Tool für HTL Notenmanagement
   Bei Schüler werden automatisch die Klasse/matrNr des Schülers ausgewählt
   Lehrer müssen bei manchen Funktionen --matrNr und --klasse den Schüler bzw die Klasse auswählen"
   #:multi
   [("--fach") fachname
               "set fachname"
               (fach fachname)]
   #:once-any
   ["--format=json" "Daten im JSON Format angezeigt"
                    (output-format "json")]
   ["--format=table"        (
                             "Daten werden im UTF-8 table format"
                             "geeignet für Unix piping")
                            (output-format "table")]
   ["--format=pptable"        (
                               "Daten als prettyprinted table anzeigen")
                              (output-format "ppable")]
   #:once-any
   [("--matrNr") matrikelNr
                 "matrikelNr setzen"
                 (matrNr (string->number matrikelNr))]
   [("--klasse") name
                 "klasse auswählen"
                 (klasse name)]
   #:once-each
   [("-l" "--login") username password
                     "Mit username und passwort anmelden"
                     (perform-login username password)
                     (action #f)]
   [("-s" "--subjects") (
                         "Fächer von Schüler ausgeben"
                         "Lehrer können mit --matrNr Schüler auswählen"
                         "wenn man als Lehrer keine matrNr angibt werden alle Fächer angezeigt"
                         )
                        (action "subjects")
                        (mandatoryarg (list matrNr "--matrNr"))]
   [("--fw")
    "frühwarnungen"
    (action "fw")]
   [("-i" "--student-info")
    ("Schüler anzeigen"
     "Lehrer können mit --matrNr Schüler auswählen."
     "Ohne Zusatzparameter bekommt man als Lehrer die Schülerliste")
    (action "dump")    (action "studentinfo")]
   [("-a" "--absences")
    ("Fehlstunden von Schüler anzeigen"
     "Lehrer müssen mit --matrNr Schüler auswählen."
     "Ohne Zusatzparameter bekommt man als Lehrer die Schülerliste")
    (action "absences")
    (mandatoryarg (list matrNr "--matrNr"))]
   [("-d" "--dump-student")
    ("JSON dump mit allen informationen von ausgewähltem Schüler"
     "Lehrer müssen mit --matrNr den Schüler auswählen"
     )
    (action "dump")
    (mandatoryarg (list matrNr "--matrNr"))]
   [("--lf")
    ("Leistungsfeststellungen einer Klasse"
     "Optional:"
     "\t --subject"
     "\t  Leistungsfeststellungen für ein Fach"
     "Lehrer müssen mit --klasse die klasse auswählen"
     )
    (action "lf")
    (mandatoryarg (list klasse "--klasse"))]
   [("--lf-grades") id
                    ("Noten von Leistungsfesstellung"
                     "Schüler können nur ihre eignen Leistungsfeststellungen auswählen"
                     "Die ID einer Leistungsfeststellung ist mit --lf zu bekommen")
                    (action "lf-grades")
                    (lf-id (string->number id))
                    ]
   [("-g" "--grades")  ("Noten von einem Schüler anzeigen"
                        "Optional mit --fach für ein Fach anzeigen."
                        "Leher müssen mit --matrNr Schüler auswählen")
                       (action "grades")
                       (mandatoryarg (list matrNr "--matrNr"))]

   #:args () (void)))

(define (session-expired? session)
  (let* ((expires-str (hash-ref session '.expires)) ; Assuming '.expires' is correct
         (expires-unix (rfc2822->unix-time expires-str)))
    (< expires-unix (current-seconds))))

(define pick-role-action
  (when (action)
    (session (read-hash-from-json-file "/tmp/nmcli-session.json"))
    (access-token (hash-ref (session) 'access_token))
    (if (session-expired? (session))
        (printf "session expired, login required\n")
        (cond
          [(equal? (hash-ref (session) 'role) "Schueler")
           (matrNr (string->number(hash-ref (session) 'matrikelNr)))
           (klasse (hash-ref (session) 'className))
           (choose-action (action))
           ]
          [(equal? (hash-ref (session) 'role) "Lehrer")
           (if (and (mandatoryarg) (not((first (mandatoryarg)))))
               [let ([last-param
                      (if (number?  (last (mandatoryarg)))
                          (number->string  (last (mandatoryarg)))
                          (last (mandatoryarg)))
                      ])
                 (printf (string-append "Missing parameters: " last-param " is mandatory \n"))]
               [choose-action (action)]

               )
           ]
          [else
           (printf "Unrecognized role\n")])
        )
    )
  )

pick-role-action