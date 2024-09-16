#lang racket
(require "suffix-tree.rkt")

(provide (all-defined-out))

; TODO 2
; Implementați o funcție care primește două cuvinte (liste
; de caractere) w1 și w2 și calculează cel mai lung prefix
; comun al acestora, împreună cu restul celor două cuvinte
; după eliminarea prefixului comun.
; ex:
; (longest-common-prefix '(#\w #\h #\y) '(#\w #\h #\e #\n))
; => '((#\w #\h) (#\y) (#\e #\n))
; Folosiți recursivitate pe coadă.
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


; TODO 3
; Implementați recursiv o funcție care primește o listă nevidă 
; de cuvinte care încep cu același caracter și calculează cel 
; mai lung prefix comun al acestora.
; Opriți căutarea (parcurgerea) în momentul în care aveți garanția 
; că prefixul comun curent este prefixul comun final.
(define (longest-common-prefix-of-list words)
  (aux1 words '()))

(define (aux1 words acc)
  (cond
    ; daca lista de cuvinte este goala, putem returna prefixul comun
    ((null? words)  acc)
    ; daca momentan prefixul comun e null, setam primul cuvant din lista drept prefix
    ; apelam functia auxiliara pentru acesta si pentru restul cuvintelor ramase
    ((null? acc) (aux1 (cdr words) (car words)))
    (else
     ; am creat o variabila pentru prefixul nou, pe care l-am gasit folosind functia implementata anterior
     ; am apelat functia pentru cuvantul curent din lista si pt prefixul gasit anterior
     (let ((new-prefix (car(longest-common-prefix (car words) acc))))
       (if (null? new-prefix)
           '()
           ;daca am gasit un prefix comun am apelat iarasi functia aux cu noul prefix, pt restul cuvintelor din lista
           (aux1 (cdr words) new-prefix))))))
   
    



;; Următoarele două funcții sunt utile căutării unui șablon
;; (pattern) într-un text cu ajutorul arborelui de sufixe.
;; Ideea de căutare este următoarea:
;; - dacă șablonul există în text, atunci există un sufix care
;;   începe cu acest șablon, deci există o cale care începe din
;;   rădăcina arborelui care se potrivește cu șablonul
;; - vom căuta ramura a cărei etichetă începe cu prima literă
;;   din șablon
;; - dacă nu găsim această ramură, șablonul nu apare în text
;; - dacă șablonul este conținut integral în eticheta ramurii,
;;   atunci el apare în text
;; - dacă șablonul se potrivește cu eticheta dar nu este conținut
;;   în ea (de exemplu șablonul "nana$" se potrivește cu eticheta
;;   "na"), atunci continuăm căutarea în subarborele ramurii
;; - dacă șablonul nu se potrivește cu eticheta (de exemplu
;;   șablonul "numai" nu se potrivește cu eticheta "na"), atunci
;;   el nu apare în text (altfel, eticheta ar fi fost "n", nu
;;   "na", pentru că eticheta este cel mai lung prefix comun al
;;   sufixelor din subarborele său)


; TODO 4
; Implementați funcția match-pattern-with-label care primește un
; arbore de sufixe și un șablon nevid și realizează un singur pas 
; din procesul prezentat mai sus - identifică ramura arborelui a
; cărei etichetă începe cu prima literă din șablon, apoi
; determină cât de bine se potrivește șablonul cu eticheta,
; întorcând ca rezultat:
; - true, dacă șablonul este conținut integral în etichetă
; - lista (etichetă, nou pattern, subarbore), dacă șablonul se
;   potrivește cu eticheta dar nu este conținut în ea
;   (ex: ("na", "na$", subarborele de sub eticheta "na")
;   pentru șablonul inițial "nana$" și eticheta "na")
; - lista (false, cel mai lung prefix comun între etichetă și
;   șablon), dacă șablonul nu s-a potrivit cu eticheta sau nu
;   s-a găsit din start o etichetă care începe cu litera dorită
;   (ex1: (false, "n") pentru șablonul "numai" și eticheta "na")
;   (ex2: (false, "") pentru etichetă negăsită)
; Obs: deși exemplele folosesc stringuri pentru claritate, vă
; reamintim că în realitate lucrăm cu liste de caractere.
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





; TODO 5
; Implementați funcția st-has-pattern? care primește un
; arbore de sufixe și un șablon și întoarce true dacă șablonul
; apare în arbore, respectiv false în caz contrar.
(define (st-has-pattern? st pattern)
  (cond
    ; daca arborele a fost parcurs si n-am gasit nimic, returnam false
    ((st-empty? st) #f)
    (else
     ; apelam functia implementata mai sus
     (let((match-result (match-pattern-with-label st pattern)))
       (cond
         ;daca functia de mai sus a intors un rezultat boolean, inseamna ca am ajuns la o concluzie si intoarcem rezultatul acesteia
         ((boolean? match-result) match-result)
         ; am verificat daca functia a intors un rezultat
         ((null? match-result) #f)
         ;daca rezultatul este o lista cu 3 elemente, atunci apelez functia curenta pentru subarborele corespunzator potrivirii partiale
         ((equal? 3 (length match-result))
          (st-has-pattern? (get-branch-subtree (caddr match-result)) (cadr match-result)))
         (else
          ; verific recursiv daca sablonul se potriveste cu eticheta din prima ramura a arborelui sau cu oricare alta ramura
          (or (st-has-pattern? (get-branch-subtree (first-branch st)) pattern)
              (st-has-pattern? (other-branches st) pattern))))))))


