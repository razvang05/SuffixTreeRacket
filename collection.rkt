#lang racket

(provide (all-defined-out))

;; În acest fișier vă definiți constructorii și
;; operatorii tipului Collection.
;; În etapele anterioare, colecțiile erau de fapt
;; liste.
;; În definițiile de mai jos, veți considera că
;; o colecție este implementată ca flux.

; Întrucât stream-cons nu este o funcție obișnuită, 
; ci este definită ca o sintaxă specială, astfel
; încât ea să nu își evalueze argumentele înainte 
; de apel (comportament pe care ni-l dorim și pentru 
; collection-cons), nu putem folosi o definiție
; de tipul
;    (define collection-cons stream-cons)
; (genul acesta de definiție generează o eroare).
; Nici varianta
;    (define (collection-cons x xs) (stream-cons x xs))
; nu este o soluție, întrucât funcțiile definite de noi
; în Racket sunt funcții stricte, iar x și xs vor fi
; evaluate înainte de a intra în corpul funcției
; collection-cons și a descoperi că ele vor fi
; argumentele unui stream-cons.
; Modul de a defini collection-cons pentru a reproduce
; întocmai comportamentul lui stream-cons este:
(define-syntax-rule (collection-cons x xs) (stream-cons x xs))
; Obs: puteți schimba numele funcției, dacă nu vă
; place "collection-cons". Este o funcție folosită doar
; de voi în fișierul etapa4.rkt, nu de checker.


; TODO
; Scrieți în continuare restul definițiilor
; (care nu necesită o sintaxă specială).
(define-syntax-rule (collection-empty? collection)
  (stream-empty? collection))

(define-syntax-rule (collection-first collection)
  (stream-first collection))

(define-syntax-rule (collection-rest collection)
  (stream-rest collection))

(define empty-collection
  (stream))

(define-syntax-rule (collection-map func col)
  (stream-map func col))

(define-syntax-rule (collection-filter func col)
  (stream-filter func col))

(define-syntax-rule (collection-length col)
  (stream-length col))

(define-syntax-rule (collection-drop col n)
  (stream-drop col n))

(define (list->stream L)
  (if (null? L)
      empty-stream
      (stream-cons (car L) (list->stream (cdr L)))))





