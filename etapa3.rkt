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
  (st-has-pattern? ((text->st text) (lambda (x) x)) pattern))
; transform textul in st si apoi caut pattern ul in el cu ajutorul lui st has pattern



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
  (define suff-text2  (get-suffixes text2))
  (define st1 (text->cst text1))
  (define (suff-text2-list result suff-text2)
    ; parcurg sufixele textului 2 
    (let iterate ((result result) (suff-text2 suff-text2))
      ; ne oprim daca nu mai sunt sufixe
      (if (null? suff-text2)
          result
          ; apelez suffix-match pt toate sufixele din lista cu ajutorul named let ului
          (iterate (cons (suffix-match st1 (car suff-text2) '()) result) (cdr suff-text2))
       )
      )
    )
  (longest-result (suff-text2-list '() suff-text2))
)

(define (suffix-match st1 pattern acc)
  ; verific potrivirea dintre st si pattern ul curent
   (define match (match-pattern-with-label st1 pattern))
         (cond
           ; daca match ul e boolean, adica true il pot adauga la rezultatul final
         ((boolean? match) (append acc pattern))
         ; daca avem ca rezultat o lista cu 3 argumente adaiga eticheta la rezultat si continuam sa cautam
         ((equal? 3 (length match))
          (suffix-match (caddr match) (cadr match) (append acc (car match))))  
         (else
          ; daca returneaza false atunci lipesc la rezultat doar bucata care se potriveste si returnez lista construita
          (append acc (cadr match))
           ))
  )
; asta gaseste lista mai lunga
(define (get-max l1 l2)
  (let* ((length1 (length l1))
    (length2 (length l2)))
  (if (< length1 length2) l2 l1)
  ))

; gasim lista cu lungime maxima
(define (longest-result lists)
  (foldl get-max '() lists) )



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
(define (repeated-substring-of-given-length text len)
  (define st (text->cst text))

  (define (find-repeated-substring st path)
    ; daca am ajuns la finalul arborelui de sufixe returnam false
    (if (st-empty? st)
        #f
      (let* (
             (label (get-branch-label (first-branch st)))
              (subtree (get-branch-subtree (first-branch st)))
              (branches (other-branches st))
              )
    (cond
      ((>= (length path)  len ) ; daca lungimea caii este egala cu lungimea subsirului dorit
       (if (> (length subtree) 1) ; verific daca exista un subarbore dupa nodul curent
           (find-repeated-substring branches path) ; daca nu exista, continuam cautarea
     (take path len) ) ) ; daca exista, am gasit un subsir si returnam calea
      (else
         (or (find-repeated-substring subtree (append path label))  ; cauta in subarborele curent
             (find-repeated-substring branches path)))))) ) ; cauta in restul arborelui de sufixe

  (find-repeated-substring st '()))





