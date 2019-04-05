#!/usr/bin/racket
#lang racket
(require racket/date)
(require racket/format)

(define filename (string->path "~/.emacs.d/.project-time.save.el"))
(define port (open-input-file (expand-user-path filename)))
(define contents (read port))
(close-input-port port)

(define (format-duration hours minutes seconds)
  (let ([hh (cond ([= 0 hours] "")
                  ([= 1 hours] "1 hour ")
                  (else (format "~a hours " hours)))]
        [mm (cond ([= 0 minutes] "")
                  ([= 1 minutes] "1 minute ")
                  (else (format "~a minutes " minutes)))]
        [ss (cond ([= 0 seconds] "")
                  ([= 1 seconds] "and 1 second")
                  (else (format "and ~a seconds" seconds)))])
    (string-trim (format "~a~a~a" hh mm ss))))

(define (format-duration-short hours minutes seconds)
  (let ([hh (cond ([= 0 hours] "")
                  (else (format "~ah" hours)))]
        [mm (cond ([= 0 minutes] "")
                  (else (format "~am" minutes)))]
        [ss (cond ([= 0 seconds] "")
                  (else (format "~as" seconds)))])
    (string-trim (format "~a~a~a" hh mm ss))))


(define (do-it)
  (let ([most-recent -inf.0]
        [lst '()])
    (for-each (lambda (e) (when (> (second e) most-recent)
                            (set! most-recent (second e))
                            (set! lst e))) contents)
    (unless (null? lst)
      (letrec ([path (first lst)]
               [filename (file-name-from-path path)]
               [last-edited (seconds->date (second lst))]
               [time-total (third lst)]
               [time-int (round (inexact->exact time-total))]
               [hours (floor (/ time-int (* 60 60)))]
               [minutes (modulo (floor (/ time-int 60)) 60)]
               [seconds (modulo time-int 60)])
        (format "~a: ~a" filename (format-duration-short hours minutes seconds))))))

(let ([out (do-it)])
  (displayln out)
  ;;(displayln out)
  ;;(displayln "#93a1a1")
  )

