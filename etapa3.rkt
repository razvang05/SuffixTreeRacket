#lang racket
(require "suffix-tree.rkt")
(require "etapa1.rkt")
(require "etapa2.rkt")

(provide (all-defined-out))

;; Această etapă este dedicată aplicațiilor 
;; arborelui de sufixe:
;; - găsirea unui șablon într-un text
;; - cel mai lung subșir comun a două texte
;; - găsirea unui subșir de lungime dată care se
;;   repetă în text
;; Conform convenției din etapele anterioare, un text
;; este întotdeauna reprezentat ca listă de caractere.
;; Rezultatele funcțiilor de mai jos sunt de asemenea
;; reprezentate ca liste de caractere.


; TODO 1
; Implementați funcția substring? care primește un text și
; un șablon nevid și întoarce true dacă șablonul apare în 
; text, respectiv false în caz contrar.
; Observație: ați implementat deja logica principală a
; acestei căutări în etapa 1, în funcția st-has-pattern?,
; care este un operator al tipului ST. Acum aveți toate
; uneltele necesare implementării operatorului corespunzător
; pentru tipul text (pentru că în etapa 2 ați implementat
; construcția arborelui de sufixe asociat unui text).
(define (substring? text pattern)
  (let* ((text-sufix (text->ast text)))
  (st-has-pattern?  text-sufix pattern)))
      


; TODO 2
; Implementați funcția longest-common-substring care primește
; două texte și determină cel mai lung subșir comun al
; acestora, folosind algoritmul următor:
; 1. Construiește arborele de sufixe ST1 pentru primul text.
; 2. Pentru fiecare sufix din al doilea text (de la cel mai
;    lung la cel mai scurt), găsește cea mai lungă potrivire 
;    cu sufixele din primul text, urmând căile relevante în ST1.
; 3. Rezultatul final este cel mai lung rezultat identificat
;    la pasul 2 (în caz de egalitate a lungimii, păstrăm
;    primul șir găsit).
; Folosiți named let pentru a parcurge sufixele.
; Observație: pentru sufixele din al doilea text nu dorim 
; marcajul de final $ pentru a nu crește artificial lungimea 
; șirului comun cu acest caracter.
; Hint: Revizitați funcția match-pattern-with-label (etapa 1).
(define (longest-common-substring text1 text2)
  ; formez arborele pentru text1 si toate sufixele pentru text2
  (let* ((st1 (text->cst text1))
         (all-suffixes (get-suffixes text2))
         (suffix-match '()))
    ; cu ajutorul lui loop iterez prin toate sufixele
    (let loop ((suffix all-suffixes))
      (if (null? suffix)
          ; daca am terminat lista de sufixe o sa sortez descrescator lista
          ; si o sa iau ca rezultat fix primul element 
          (let* ((suffix-sort(sort (reverse suffix-match) (lambda (a b) (> (length a) (length b))))))
                      (if (null? suffix-sort) '()
                          (car suffix-sort)))
         ;pentru fiecare sufix apelez functia de cautare find-max-match
          (let* ((curr-suffix (car suffix))
                 (res-fav (find-max-match st1 curr-suffix)))
          ; aici adaug la lista buna de sufixe rezultatele favorabile
          (if(not(null? res-fav))
            (set! suffix-match (cons res-fav suffix-match)) suffix-match)
            (loop (cdr suffix)))))))

(define (find-max-match tree curr-suffix)
  ; verific pe rand cazurile lui match-pattern
  ; si cu search verific in special acel caz cand trebuie sa cautam in continuare
  ; parcurgere sufix named let
  (let search ((curr-st tree) (curr-suf curr-suffix))
                           (let* ((res (match-pattern-with-label curr-st curr-suf)))
                             (cond
                               ((equal? res #t) curr-suf)
                               ((equal? (car res) #f) (cadr res))
                               (else (let* ((subtree(caddr res))
                                            (match-partial(car res))
                                            (new-text(cadr res)))
                                         (append match-partial (search subtree new-text))))))))
  
; TODO 3
; Implementați funcția repeated-substring-of-given-length
; care primește un text și un număr natural len și
; parcurge arborele de sufixe al textului până găsește un
; subșir de lungime len care se repetă în text.
; Dacă acest subșir nu există, funcția întoarce false.
; Obs: din felul în care este construit arborele de sufixe
; (pe baza alfabetului sortat), rezultatul va fi primul 
; asemenea subșir din punct de vedere alfabetic.
; Ideea este următoarea: orice cale în arborele de sufixe
; compact care se termină cu un nod intern (un nod care 
; are copii, nu este o frunză) reprezintă un subșir care
; se repetă, pentru că orice asemenea cale reprezintă un
; prefix comun pentru două sau mai multe sufixe ale textului.
; Folosiți interfața definită în fișierul suffix-tree
; atunci când manipulați arborele.

(define (find-in-branch branch subtree len)
  (let ((label (get-branch-label branch)))
    (if (>= (length label) len)
        ; daca lungimea etichetei se incadreaza in interval o returnam
        (take label len)
        ; daca nu continuam cautarea
        ; pentru a ne apropia de dimensiunea len
        (let ((result (find-text subtree (- len (length label)))))
          (if result
              ; daca rezultatul e valid il concatenam
              (append label result)
              #f)))))

(define (find-text st len)
  ; daca arborele este vid returnez false
  (if (st-empty? st)
      #f 
      (let* ((branch (first-branch st))
             (subtree (get-branch-subtree branch))
             (other-branch (other-branches st))
             (len-branch (length branch)))
        ; daca ramura nu este vida si contine cel putin 2 etichete
        ; ne continuam cautarea pe ramura 
        (if (and (not (null? branch)) (> len-branch 1))
            (let ((result (find-in-branch branch subtree len)))
              (if result
                  result
                  ; daca rezultatul nu e valid ne continuam cautarea pe alte ramuri
                  (find-text other-branch len)))
            (find-text other-branch  len)))))

(define (repeated-substring-of-given-length text len)
  (let ((st (text->cst text)))
    ; incepem cautarea pe arborele st 
    (find-text st len)))
      

