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
  (aux w1 w2 '()))

(define (aux w1 w2 acc)
  (cond
    ;daca am terminat de parcurs unul dintre cuvinte, inseamna ca am gasit prefixul comun si il putem returna, alaturi de restul cuvintelor
    ((or (null? w1) (null? w2)) (list (reverse acc) w1 w2))
    ;daca primele litere din cele 2 cuvinte sunt egale, continuam cautarea prefixului
    ;apelam functia auxiliara, eliminand prima litera din cele 2 cuvinte si adaugand litera similara la prefix
    ((char=? (car w1) (car w2)) (aux (cdr w1) (cdr w2) (cons (car w1) acc)))
    ;altfel returnam prefixul gasit
    (else (list (reverse acc) w1 w2) )
    )
  )

; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection
(define (longest-common-prefix-of-collection words)
  (aux1 words '()))

(define (aux1 words acc)
  (cond
    ; daca lista de cuvinte este goala, putem returna prefixul comun
    ((collection-empty? words)  acc)
    ; daca momentan prefixul comun e null, setam primul cuvant din lista drept prefix
    ; apelam functia auxiliara pentru acesta si pentru restul cuvintelor ramase
    ((null? acc) (aux1 (collection-cdr words) (collection-car words)))
    (else
     ; am creat o variabila pentru prefixul nou, pe care l-am gasit folosind functia implementata anterior
     ; am apelat functia pentru cuvantul curent din lista si pt prefixul gasit anterior
     (let ((new-prefix (car(longest-common-prefix (collection-car words) acc))))
       (if (null? new-prefix)
           '()
           ;daca am gasit un prefix comun am apelat iarasi functia aux cu noul prefix, pt restul cuvintelor din lista
           (aux1 (collection-cdr words) new-prefix))))))
   


(define (match-pattern-with-label st pattern)
   ;am creat o variabila pentru a gasi ramura a carei etichete incepe cu prima litera din sablon
    (let ((branch (get-ch-branch st (car pattern))))
      (cond
        ;cazul in care nu am gasit eticheta
        ((not branch)
         (list #f '()))
        ;daca sablonul este acelasi cu eticheta ramurii, returnez true
        ((equal? pattern (get-branch-label branch))
         #t)
        (else
         ; am creat variabile pentru prefixul comun dintre eticheta ramurii si sablonul dat
         ; si pentru partea din sablon care nu este continuta in prefixul comun
         (let* ((common-prefix (car (longest-common-prefix  (get-branch-label branch) pattern))) 
                (new-pattern (cddr (longest-common-prefix  (get-branch-label branch) pattern)))) 
           (cond
             ; daca sablonul este acelasi cu prefixul gasit returnez true
             ((equal? common-prefix pattern) #t)
             ; daca eticheta este aceeasi cu prefixul gasit, inseamna ca sablonul se potriveste cu eticheta, dar nu e continut in ea 
             ((equal? common-prefix (get-branch-label branch)) (list (get-branch-label branch) (car new-pattern) (get-branch-subtree branch)))
           (else
            ;altfel am returnat false si cel mai lung prefix comun
            (list #f common-prefix)
            )
           )))))) 

(define (st-has-pattern? st pattern)
  (cond
    ; daca arborele a fost parcurs si n-am gasit nimic, returnam false
    ((st-empty? st) #f)
    (else
     ; apelam functia implementata mai sus
     (let((match-result (match-pattern-with-label st pattern)))
       (cond
         ;daca functia de mai sus a intors un rezultat boolean, inseamna ca am ajuns la o concluzie si intoarcem rezultatul acesteia
         
         ((eq? match-result #t) #t)
         ; am verificat daca functia a intors un rezultat
         ((null? match-result) #f)
         ;daca rezultatul este o lista cu 3 elemente, atunci apelez functia curenta pentru subarborele corespunzator potrivirii partiale
         ((equal? 3 (length match-result))
          (st-has-pattern?  (caddr match-result) (cadr match-result)))
         (else
          ; verific recursiv daca sablonul se potriveste cu eticheta din prima ramura a arborelui sau cu oricare alta ramura
          #f))))))

(define (get-suffixes text)
  (if (collection-empty? text)
       '()
      (collection-cons text (get-suffixes(collection-cdr text)))
   ))


(define (get-ch-words words ch)
  (collection-filter (lambda (word)  (and (not (collection-empty? word))(equal? (collection-car word) ch)))
           words) 
)


(define (ast-func suffixes)
  (if (or (collection-empty?  suffixes) (collection-empty?  (collection-car suffixes)))
       (collection-empty-stream)
  (let ((prefix  (collection-car (collection-car suffixes))))
    (cons (list prefix)
           (collection-map (lambda (suf) (cdr suf)) suffixes))))
  )


(define (cst-func suffixes)
   (if (collection-empty? suffixes)
      (collection-empty-stream)
  (let ((prefix (longest-common-prefix-of-collection suffixes)))
    (cons prefix
          (collection-map (lambda (suf) (caddr (longest-common-prefix  prefix suf))) suffixes)))))


; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)



(define (suffixes->st labeling-func suffixes alphabet)
  (let (
         (bahrains
           (collection-map (lambda (ch) (if (collection-empty? (get-ch-words suffixes ch)) '() (labeling-func  (get-ch-words suffixes ch))))
                 alphabet)))
    (collection-map (lambda (bahrain)
            (if (not (null? bahrain))
                  (cons  (get-branch-label bahrain)
                        (if (collection-empty? (get-branch-subtree bahrain))
                            collection-empty-stream
                            (suffixes->st labeling-func (get-branch-subtree bahrain) alphabet)))
                '()
                ))
         (collection-filter (lambda (bahrain) (not (null? bahrain))) bahrains))))



(define (text->st text)
  (lambda (labeling-func)
    (let ((suf (get-suffixes (append text '(#\$)))))
    (suffixes->st labeling-func
                 suf
                  (list->stream(sort (remove-duplicates (cons #\$  text) equal?) char<?)))
    )))


(define (list->stream lst)
  (if (null? lst)
      collection-empty-stream
      (collection-cons (car lst) (list->stream (cdr lst)))))


(define (text->ast text)
    ((text->st text) ast-func))

(define (text->cst text)
    ((text->st text) cst-func))


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (substring? text pattern)
  (st-has-pattern? (text->ast text) pattern))


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
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
       (if (not (st-empty? subtree)) ; verific daca exista un subarbore dupa nodul curent
           (find-repeated-substring branches path) ; daca nu exista, continuam cautarea
     (take path len) ) ) ; daca exista, am gasit un subsir si returnam calea
      (else
         (or (find-repeated-substring subtree (append path label))  ; cauta in subarborele curent
             (find-repeated-substring branches path)))))) ) ; cauta in restul arborelui de sufixe

  (find-repeated-substring st '()))