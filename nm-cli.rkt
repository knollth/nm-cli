#! /usr/racket/bin/racket

#lang racket

(require net/http-easy)
(require json)
(require text-table)
(require json/format/simple)
(require json)

(define url "https://notenmanagement.htl-braunau.at/rest")

;; parameter My-Name is one of:
;; - #false
;; - String
(struct lehrerInfo (lehrerID localLogin))


(define access-token (make-parameter #f))
(define username (make-parameter #f))
(define password (make-parameter #f))
(define session (make-parameter #f))
(define output-format (make-parameter "table"))
(define fach (make-parameter #f))
(define action (make-parameter #f))
(define matrNr (make-parameter #f))
(define fachID (make-parameter #f))


(define (authenticated-request endpoint callback)
  (if (access-token)
      (let ([auth-procedure (bearer-auth (access-token))]
            [full-url (string-append url endpoint)])
        (callback full-url auth-procedure))
      (error "No access token available.")))

(define (make-request url auth)
  (let ([response (get url
                       #:auth auth
                       #:headers '#hash((Content-Type . "application/x-www-form-urlencoded")))])
    (response-json response)))

(define (getGradesForStudent matrNr  [subject #f])
  (cond
    [(fach)
     (authenticated-request (string-append "/api" "/Schueler/" (number->string matrNr) "/Faecher/" (fach) "/Noten")
                            make-request)]
    [else
     (authenticated-request (string-append "/api" "/Schueler/" (number->string matrNr) "/Noten?sort=Fach")
                            make-request)]))



;; WIP
(define (getGradesForKlasse klassenName [subject #f])
  (cond
    [(not subject)
     (authenticated-request (string-append "/api" "/Schueler/" (number->string matrNr) "/Noten?sort=Fach")
                            make-request)]
    [else
     (authenticated-request (string-append "/api" "/Schueler/" (number->string matrNr) "/Faecher/" subject "/Noten")
                            make-request)]))



(define (getSubjects [matrNr #f])
  (let ([url (if matrNr
                 (string-append "/api" "/Schueler/" (number->string matrNr) "/Faecher")
                 (string-append "/api" "/Faecher"))])
    (authenticated-request url
                           (lambda (url auth)
                             (let ([response (get url
                                                  #:auth auth
                                                  #:headers '#hash((Content-Type . "application/x-www-form-urlencoded")))])
                               (response-json response))))))

(define (getAbsences matrNr)
  (authenticated-request (string-append "/api" "/Schueler/" (number->string matrNr) "/Fehlstunden")
                         (lambda (url auth)
                           (let ([response (get url
                                                #:auth auth
                                                #:headers '#hash((Content-Type . "application/x-www-form-urlencoded")))])
                             (response-json response)))))

(define (getStudent [matrNr #f])
  (let ([url (if matrNr
                 (string-append "/api" "/Schueler/" (number->string matrNr))
                 (string-append "/api" "/Schueler"))])
    (authenticated-request url
                           (lambda (url auth)
                             (let ([response (get url
                                                  #:auth auth
                                                  #:headers '#hash((Content-Type . "application/x-www-form-urlencoded")))])
                               (response-json response))))))

(define (getFWKlasse [fach #f])
  (let ([url (if matrNr
                 (string-append "/api" "/Schueler/" (number->string matrNr))
                 (string-append "/api" "/Schueler"))])
    (authenticated-request url
                           (lambda (url auth)
                             (let ([response (get url
                                                  #:auth auth
                                                  #:headers '#hash((Content-Type . "application/x-www-form-urlencoded")))])
                               (response-json response))))))

(define (getFW [matrNr #f] [klasse #f] [fach #f])
  (let
      ([url (cond
              ;; Both matrNr and klasse are set, which is not allowed
              [(and matrNr klasse) (error "Cannot have both matrNr and klasse set")]
              ;; Only klasse is set
              [(and klasse (not fach)) (string-append "/api/Klassen/" klasse "Fruehwarnungen")]
              ;; Klasse and fach are both set
              [(and klasse fach) (string-append "/api/Klassen/" klasse "/fach/" fach)]
              ;; Only matrNr is set
              [matrNr (string-append  "/api/Schueler/" (number->string matrNr) "Fruehwarnungen")]
              ;; Only fach is set, which is not allowed
              [fach (error "Setting only fach is not allowed")]
              ;; Nothing is set, use a default URL
              (else (string-append "/api/Schueler")))])
    (authenticated-request url)))




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
                                                               (cons 'id (hash-ref grade 'LF_ID))
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
  (if (null? hashes)
      '()
      (let* ((keys (hash-keys (first hashes)))
             (rows (map (lambda (hash)
                          (map (lambda (key)
                                 (hash-ref hash key))
                               keys))
                        hashes)))
        (cons keys rows))))


(define (print-data data)
  (cond
    [(equal? (output-format) "json")
     (pretty-print-jsexpr  data)]
    [(equal? (output-format) "ppable")
     (printf "printing as pretty table\n")
     (print-table  (hashes-to-lists data))]
    [else
     (printf "printing as simple table\n")
     (print-simple-table  (hashes-to-lists data))]
    )
  )


(define (make-login-request username password)
  (define params `((grant_type . "password")
                   (username . ,username)
                   (password . ,password)))
  (define response-data (response-json(post (string-append url "/token")
                                            #:form params)))
  response-data)

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

(define (process-action actionfun)
  (let* ([data(actionfun (matrNr))]
         [data-list (if (list? data) data (list data))]) ; Wrap in list if not already a list
    (print-data data-list)))


(define (choose-action action)

  (case action
    [("subjects") (process-action getSubjects)]
    [("absences") (process-action getAbsences)]
    [("grades") (process-action getGradesForStudent)]
    [("fw") (process-action getFW)]
    [("studentinfo") (process-action getStudent)]
    [("dump") (pretty-print-jsexpr (generateStudentInfo (matrNr))) ]
    [else 'unknown]))

;; command line parser
(define parser
  (command-line
   #:usage-help
   "Have the computer greet you!"
   #:multi
   [("--fach") fachname
               "set fachname"
               (fach fachname)]
   [("--matrNr") matrikelNr
                 "set matrikelNr"
                 (matrNr (string->number matrikelNr))]
   [("--fachID") fachid
                 "set fachID"
                 (fachID fachid)]
   #:once-any
   ["--format=json" "Display data in JSON Format"
                    (output-format "json")]
   ["--format=table"        (
                             "Display data in a UTF-8 table format"
                             "useful for piping")
                            (output-format "table")]
   ["--format=pptable"        (
                               "Display data in prettyprinted table")
                              (output-format "ppable")]
   #:once-each
   [("-l" "--login") username password
                     "Login with username and password"
                     (action #f)
                     (define loginres (make-login-request username password))
                     (access-token (hash-ref loginres 'access_token))
                     (session loginres)
                     (write-hash-to-json-file loginres "/tmp/nmcli-session.json")
                     (printf "~a\n" (string-append "logged in as: " (hash-ref (session) 'role)
                                                   (if (hash-has-key? (session) 'matrikelNr)
                                                       (string-append " matrikelNr: " (hash-ref (session) 'matrikelNr))
                                                       "")))]
   [("-s" "--subjects")
    "get subjects"
    (action "subjects")]
   [("-f" "--fruehwarnungen")
    "get frühwarnungen"
    (action "fw")]
   [("-i" "--student-info")
    "get student list"
    (action "studentinfo")]
   [("-a" "--absences")
    "get absences for student"
    (action "absences")]
   [("-d" "--dump-student")
    "JSON dump of student with all Subjects and LFs as JSON"
    (action "dump")]

   [("-g" "--grades")  ("Get grades for a student."
                        "Teachers only: Optionally specify matrNr and/or fachID.")
                       (action "grades")]

   #:args () (void)))

(when (action)
  (session (read-hash-from-json-file "/tmp/nmcli-session.json"))
  (access-token (hash-ref (session) 'access_token))
  (cond
    [(equal? (hash-ref (session) 'role) "Schueler")
     (matrNr (string->number(hash-ref (session) 'matrikelNr)))
     (choose-action(action))]
    [(equal? (hash-ref (session) 'role) "Lehrer")
     (choose-action(action))]
    [else
     (printf "Unrecognized role\n")])
  )