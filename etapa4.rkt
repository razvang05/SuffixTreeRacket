#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")

(provide (all-defined-out))

;; Vom prelua toate funcțiile din etapele 1-3 (exceptând
;; longest-common-substring, care nu beneficiază de 
;; reprezentarea ca flux întrucât parcurge tot arborele)
;; și le vom adapta la noua reprezentare a unui ST.
;;
;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de 
;; abstractizare), ar trebui să alterăm doar funcția 
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi 
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".


; TODO
; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în 
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).

(define (longest-common-prefix w1 w2)
  ( long-common-prefix-help w1 w2 '()))
(define (long-common-prefix-help w1 w2 acc)
  (cond
    ; verific daca listele au ajuns la final
    ; daca da, afisez rezultatul
    ((or (null? w1) (null? w2)) (list (reverse acc) w1 w2))
    ; parcurg cele 2 cuvinte si daca au elementele egale
    ; apelez recursiv functia si adaug la acc valoarea dorita
    ((equal? (car w1) (car w2)) (long-common-prefix-help (cdr w1) (cdr w2) (cons (car w1) acc)))
    (else ( list (reverse acc) w1 w2))))


; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection
(define (longest-common-prefix-of-collection words)
  (cond
    ((collection-empty? (collection-rest words)) (collection-first words))
    ; fac prefixul la fiecare pas intre 2 elemente si apoi rezultatului
    ; ii fac iar prefixul cu urmatorul apeland recursiv functiile
    ((collection-first (longest-common-prefix (collection-first words) (longest-common-prefix-of-collection (collection-rest words)))))))

(define (match-pattern-with-label st pattern)
  (let* ((pattern (if (list? pattern) pattern (list pattern)))
         (branch (get-ch-branch st (car pattern))))
         
    (cond
      ; daca ramura exista continuam
      ( (and (not (null? branch)) (not(equal? branch #f)))
        (let* (( label (get-branch-label branch))
               ( prefix (longest-common-prefix pattern label)))
          (cond
            ; prin verificarea prefixului comun si sablon verific daca
            ; sablonul se afla integral in eticheta
            ( (or (equal? pattern label) (equal? (car prefix) pattern)) true)
            ; daca eticheta se afla in sablon
            ; voi intoarce eticheta,sablonul fara primele caractere pe care face match cu label
            ; si subarborele pentru a sti de unde am ramas
            ((equal? (car prefix) label) (list label (drop pattern (length label)) (get-branch-subtree branch)))
            (else ( list false (car prefix)))
            )))
      (else (list false '())))))


(define (st-has-pattern? st pattern)
 ; aplic functia anterioara si stochez rezultatul in result
  (let* ((result (match-pattern-with-label st pattern)))
    (cond
        ; daca rezultatul este true adica daca pattern
        ; s a gasit integral in arbore
      ( (equal? result #t) #t)
      ; verific daca restul sablonului este in arbore apeland recursiv functia
      ; daca primul termen din result este #f atunci mergem pe else
      ( (and (list? result) (not (eq? (car result) #f))) (st-has-pattern? (caddr result) (cadr result)))
      (else #f))))


(define (get-suffixes text)
  (if (collection-empty? text) empty-collection
      ; apelez recursiv mereu fara primul caracter si rezulta sufixul
      (collection-cons text (get-suffixes (collection-rest text)))))


(define (get-ch-words words ch)
  ; filtrez pw words luand doar acele cuvinte care respecta conditia
  (collection-filter (lambda (word)
            (and (not (collection-empty? word)) (equal? ch (car word))))
          words))

(define (ast-func suffixes)
  (cons (list (collection-first(collection-first suffixes))) (collection-map (lambda (sufix) (collection-rest sufix)) suffixes)))


(define (cst-func suffixes)
  (let ((common-sufix (longest-common-prefix-of-collection suffixes)))
    ( cons common-sufix
           ;pentru fiecare sufix elimin primele n caractere care sunt ale prefixului comun
           (collection-map (lambda (sufix)
                  (drop sufix (length common-sufix)))
                suffixes))))


; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)
(define (suffixes->st labeling-func suffixes alphabet)
(let* ((list-sufixes (collection-map (lambda (character)
                              ; iau sufixele care au acelasi caracter 
                              (let* ((suffixes-with-same-character(get-ch-words suffixes character)))
                                (if (collection-empty? suffixes-with-same-character) #f 
                                    ;execut functia ast cau cst 
                                    (let* (( func-execut (labeling-func suffixes-with-same-character))
                                           (prefix (car func-execut))
                                           ; iau restu subarborilor 
                                           (other-suffixes (cdr func-execut)))
                                      ; in caz ca mai exista noi sufixe mai aplic din nou acelasi procedeu
                                      ; in caz contrar pun la lista "prefix" ul
                                      (if (not(collection-empty? other-suffixes)) (cons prefix (suffixes->st labeling-func other-suffixes alphabet))
                                          (prefix))))))alphabet)))
    ; filtrez doar valorile diferie de false
    (collection-filter (lambda (invalid) (not(eq? invalid #f))) list-sufixes)))


; nu uitați să convertiți alfabetul într-un flux
(define text->st
  (lambda (labeling-func)
    (lambda (text)
      ; adaug la finalul textului $ apoi formez alfabetul
      (let* ((text-suffixes(append text '(#\$)))
             ; eliminand elementele duplicate si apoi sortandu le
             (alfa-sort(list->stream(sort (remove-duplicates text-suffixes) char<?)))
             ; in st-final este rezultatul aplicarii functiei suffixes-st cu parametrii corespunzatori
             (st-final (suffixes->st labeling-func (get-suffixes text-suffixes) alfa-sort))) st-final))))


(define text->ast
  (text->st ast-func))


(define text->cst
 (text->st cst-func))


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (substring? text pattern)
  (let* ((text-sufix (text->ast text)))
  (st-has-pattern?  text-sufix pattern)))


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (repeated-substring-of-given-length text len)
  (let ((st (text->cst text)))
    ; incepem cautarea pe arborele st 
    (find-text st len)))

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
  (if (st-empty? st)
      #f
      (let* ((branch (first-branch st))
             (subtree (get-branch-subtree branch))
             (other-branches (other-branches st)))
        (if (not (st-empty? subtree))
            (let ((result (find-in-branch branch subtree len)))
              (if result
                  result
                  (find-text other-branches len)))
            (find-text other-branches len)))))
