;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname arbit_exercise) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require racket/string)
(require 2htdp/batch-io)
;Data Definition
; A List-of-amounts is one of:
; – empty
; – (cons PositiveNumber List-of-amounts)
; interp. a List-of-amounts represents some amounts of money

(define PN1(list 1 2 3 4))
(define PN2(list 2 3 4 5))

; A List-of-temperatures is one of:
; – (cons CTemperature empty)
; – (cons CTemperature List-of-temperatures)
; interp: non-empty list of measured temperatures
 
; A CTemperature is a Number greater or equal to -256. 


; Function
;ListOfNatural -> Natural
; Produces sum of numbers within a List
;(check-expect(sum empty)0)
(check-expect(sum PN1)10)
(check-expect(sum PN2)14)

#;(define (sum lon)0) ;stub

(define (sum lon)
  (cond[(empty? (rest lon)) (first lon)]
       [else
        (+ (first lon)
           (sum(rest lon)))]))

;; ListOfNaturals -> Boolean
; Produces true if all # in list are positive false otherwise
(check-expect(pos? empty)true)
(check-expect(pos? (list 1 2 3 4 5))true)
(check-expect(pos? (list -1 2 3 4 5))false)


(define (pos? lon)
  (cond[(empty? lon) true]
       [else
        (if(> 0 (first lon))
           false
           (pos? (rest lon)))]))


; List-of-temperatures -> Number
; compute the average temperature
(check-expect(average (list 1 2 3))2)
(check-expect(average (list 1 ))1)
(check-expect(average (list 1 2 3))2)

(define (average alot)
  (/ (sum alot) (how-many alot)))

; List)fTemperature -> Number
; count the temperatures on the given list
(check-expect(how-many (list 3 4))2)
(define (how-many alot)
  (cond[(empty? (rest alot)) 1]
       [else
        (+ 1
           (how-many (rest alot))) ]))

; N String -> List-of-strings
; create a list of n strings s
 
(check-expect (copier 2 "hello") (cons "hello" (cons "hello" empty)))
(check-expect (copier 0 "hello") empty)

(define (copier n s)
  (cond[(zero? n) empty]
       [(positive? n) (cons s(copier (sub1 n) s))]))

; N -> Number
; compute (+ n pi) without using +
 
(check-within (add-to-pi 3) (+ 3 pi) 0.001)
 
(define (add-to-pi n)
  (cond[(zero? n) pi]
       [else
        (add1(add-to-pi (sub1 n)))]))

; Exercise 138
; Natural Image -> Image
; It produces a column—a vertical arrangement—of Natural copies of Image
(check-expect(col 0 (ellipse 20 20 "solid" "grey"))(square 0 "solid" "white"))
(check-expect(col 3 (ellipse 20 20 "solid" "grey"))(above (ellipse 20 20 "solid" "grey") (ellipse 20 20 "solid" "grey") (ellipse 20 20 "solid" "grey") (square 0 "solid" "white")))

(define (col n i )
  (cond[(zero? n) (square 0 "solid" "white")]
       [else
        (above i
               (col (sub1 n) i))]))

; Natural Image -> Image
; produces a row—a horizontal arrangement—of Natural copies of Image
(check-expect(row 0 (ellipse 20 20 "solid" "grey"))(square 0 "solid" "white"))
(check-expect(row 3 (ellipse 20 20 "solid" "grey"))(beside (ellipse 20 20 "solid" "grey") (ellipse 20 20 "solid" "grey") (ellipse 20 20 "solid" "grey") (square 0 "solid" "white")))

(define (row n i )
  (cond[(zero? n) (square 0 "solid" "white")]
       [else
        (beside i
               (row (sub1 n) i))]))

; List-of-numbers -> List-of-numbers
; compute the weekly wages for all given weekly hours
(check-expect(wage* empty)empty)
(check-expect(wage* (cons 28 empty))(cons 336 empty))
(check-expect(wage* (cons 28 (cons 40 empty)))(cons 336 (cons 480 empty)))
(check-error(wage* (cons 102 (cons 40 empty)))"b")
  


#;(define(fn-for-lon lon)
  (cond[(empty? lon (...))]
       [else
        (... (first lon)
             (fn-for-lon(rest lon)))]))

(define(wage* lon)
  (cond[(empty? lon) empty]
       [(< 100 (first lon)) (error "b")]
       [else
        (cons (* 12 (first lon))
             (wage*(rest lon)))]))

;; ListOfString -> ListOfString
;; replaces all occurrences of "robot" with "r2d2"
(check-expect(subst-robot empty)empty)
(check-expect(subst-robot (cons "toy" empty))(cons "toy" empty))
(check-expect(subst-robot (cons "toy" (cons "robot"empty)))(cons "toy" (cons "r2d2" empty)))


(define(subst-robot lon)
  (cond[(empty? lon) empty]
       [else
        (cons (string-replace (first lon) "robot" "r2d2")
             (subst-robot(rest lon)))]))
             
(define-struct work (employee rate hours))
; Work is a structure: (make-work String Number Number).
; interp. (make-work n r h) combines the name (n)
; with the pay rate (r) and the number of hours (h) worked.

; Low (list of works) is one of:
; – empty
; – (cons Work Low)
; interp. an instance of Low represents the work efforts
; of some hourly employees

#;(define (fn-for-work w)
    (...(work-employee w)
        (work-rate w)
        (work-hour w)))

; Low -> ListOfNumbers
; compute the weekly wages for all given weekly work records
(check-expect(low empty)empty)
(check-expect(low (cons (make-work "b" 11.95 39) empty))(cons (* 11.95 39) empty))
(check-expect(low (cons (make-work "b" 11.95 39) (cons (make-work "m" 12.95 45) empty ))) (cons (* 11.95 39) (cons (* 12.95 45) empty)))

(define(low lon)
  (cond[(empty? lon) empty]
       [else
        (cons (getpay(first lon))
             (low(rest lon)))]))

(define (getpay w)
    (* (work-rate w)
        (work-hours w)))

(define-struct pons (x y))
;pons (make-pons Natural Natural)
;interp as a position with x and y coordinates
(check-expect(sumx empty)0)
(check-expect(sumx (cons p0 empty))1)
(check-expect(sumx (cons p0 (cons p1 empty)))3)
(define p0(make-pons 1 2))
(define p1(make-pons 2 4))
(define p2(make-pons 3 6))
(define p3(make-pons 101 6))
(define p4(make-pons 1 205))

#;(define (fn-for-pons w)
    (...(pons-x w)
        (pons-y w)))

(define (getx w)
    (pons-x w))

(define (gety w)
    (pons-y w))

(define (sumx lon)
  (cond[(empty? lon) 0]
       [else
        (+ (getx(first lon))
           (sumx(rest lon)))]))
;ListOfPons -> ListOfPons
; produces a Posns whose x coordinates are between 0 and 100 and whose y coordinates are between 0 and 200.
(check-expect(legal empty)empty)
(check-expect(legal (cons p0 empty))(cons p0 empty))
(check-expect(legal (cons p0 (cons p3 empty))) (cons p0 empty))
(check-expect(legal (cons p0 (cons p3 (cons p4 (cons p1 empty))))) (cons p0 (cons p1 empty)))
(check-expect(legal (cons p0 (cons p4 (cons p1 empty)))) (cons p0 (cons p1 empty)))
(define(legal lon)
  (cond[(empty? lon) empty]
       [(< 100 (getx(first lon)))(legal(rest lon)) ]
       [(< 200 (gety(first lon))) (legal(rest lon))]
       [else
        (cons (first lon)
             (legal(rest lon)))]))

; Exercise 157

;; ListOfString is one of
;; - empty
;; - (cons String ListOfString)
;  interp. list of Strings
(define L0 empty)
(define L1 (cons "wow" empty))
(define L2 (cons "wow" (cons "wait" empty)))

#;(define (fn-for-los los)
  (cond[(empty? los) (...)] (first los)			;String
       [else
        (... (first los)
        (fn-for-los (rest los)))]))

;; ListOfListOfString is one of
;; - empty
;; - (cons (cons String ListOfString) ListOfListOfString)
;  interp. List of List of Strings

#;(define (fn-for-los los)
  (cond[(empty? los) (...)] (first los)			;String
       [else
        (... (first los)
        (fn-for-los (rest los)))]))

(define LL0 empty)
(define LL1 (cons (list "wow") empty))
(define LL2 (cons (list "wow") (list "test" "this")))


;; ListOfString -> String
;  produces a concatinated string of strings in list
(check-expect (catlist (list "Put" "up" "in" "a" "place"))(string-append "Put " "up " "in " "a " "place "))
(check-expect (lapse (list (list "Put" "up" "in" "a" "place") (list "Put" "up" "in" "a" "place")))(string-append (string-append "Put " "up " "in " "a " "place ") "\n" (string-append "Put " "up " "in " "a " "place ") "\n" ))
(check-expect (lapse (list (list "Put" "up" "in" "a" "place") empty (list "Put" "up" "in" "a" "place")))(string-append (string-append "Put " "up " "in " "a " "place ") "\n" (string-append "Put " "up " "in " "a " "place ") "\n" ))

;(define (catlist los)empty)

(define (catlist los)
  (cond[(empty? los) "" ] 
       [else
        (string-append (string-append (first los) " ")
        (catlist (rest los)))]))


(define (lapse lls)
  (cond [(empty? lls) ""]
        [(empty? (first lls)) (lapse (rest lls))]
        [else
         (string-append (string-append(catlist (first lls)) "\n")
              (lapse (rest lls)))]))

; exersice 158
(define n "ttt.txt")
;; ListOfString -> ListOfString
;; produces list with selected artical removed
(check-expect(no-articles (list "Put" "up" "in" "a" "place")) (string-append"Put " "up " "in " "place " "\n"))
(check-expect(no-articles empty)"\n")
(check-expect(loop (list(list "Put" "up" "in" "a" "place"))) (string-append "Put " "up " "in " "place " "\n"))
(define (no-articles los)
  (cond[(empty? los) "\n" ]
       [(or (string=? (first los) "a") (string=? (first los) "an") (string=? (first los) "the")) (no-articles (rest los)) ]
       [else
        (string-append (string-append (first los) " ")
        (no-articles (rest los)))]))


(define (loop lls)
  (cond [(empty? lls) ""]
        [else
         (string-append (no-articles(first lls))   
              (loop (rest lls)))]))

;exercise 159 
; -- didn't finish but did the novel part. now just need to process through the text file
;; ListOfString -> String
;  produces a concatinated string of strings in list
#;(define (words los)
  (cond[(empty? los) (...)] (first los)			;String
       [else
        (... (first los)
        (fn-for-los (rest los)))]))

;; LLString -> String
;  produces a concatinated string of strings in list
#;(define (lines lls)
  (cond[(empty? los) (...)] (first los)			;String
       [else
        (... (first los)
        (fn-for-los (rest los)))]))

;; 1String -> 1String
;; produces encoded value
(define (encode-letter s)
  (cond
    [(< (char->integer s) 10) "00" ]
    [(< (char->integer s) 100) "01" ]
    [else "02"]))

;; String -> String
;; produces a string with encoded value
(check-expect(encode-string "pace" 4)"02010102")

(define (encode-string word len)
  (cond[(zero? len)""]
      [else
       (string-append (encode-letter (string-ref word 0))
                      (encode-string (substring word 1) (sub1 len)))]))

;exercise 160
;; didn't finish now I need to make it so that a whole text file can be processed.
; ListOfString-> Number
; Produce count of each letter
(check-expect(count-word (list "Put" "up" "in" "a" "place"))5)
(check-expect(count-char-word (list "Put" "up" "in" "a" "place"))13)
(check-expect(sum-lines (list (list "Put" "up" "in" "a" "place") empty empty (list "Put" "up" "in" "a" "place")))2)

(define (count-char-word los)
  (cond[(empty? los) 0] 			;String
       [else
        (+ (string-length(first los))
        (count-char-word (rest los)))]))

(define (count-word los)
  (cond[(empty? los) 0] 			;String
       [else
        (+ 1
        (count-word (rest los)))]))

(define (sum-lines llos)
  (cond[(empty? llos) 0] 
       [(empty? (first llos)) (sum-lines (rest llos))]
       [else
        (+ 1
        (sum-lines (rest llos)))]))

; ListOfString-> Number
; Produce count all the characters 
(define (sum-word-count los)
  (cond[(empty? los) 0] 
       [else
        (+ (count-word(first los))
        (sum-word-count (rest los)))]))

; exercise 171

(define-struct email (from date message))
; A Email Message is a structure:
; – (make-email String Number String)
; interp. (make-email f d m) represents text m sent by
; f, d seconds after the beginning of time 

#;(define (fn-for-email e)
   (... (email-from e)
        (email-date e)
        (email-message e)))

;; ListOfEmails is one of:
;; -empty
;; - (cons email ListOfEmails)

#;(define (fn-for-los los)
  (cond[(empty? los) (...)] 			;String
       [else
        (... (first los)
        (fn-for-los (rest los)))]))
(define E0 (make-email "a" 14 "wow"))
(define E1 (make-email "a" 15 "whe"))
(define E2 (make-email "di" 2 "wow"))
;-----------------------------------
;; Email -> Integer
(define (get-date e)
   (email-date e))

;; ListOfEmail -> ListOfEmail
;; Produces sorted list of e-mails from unsorted list
;;
(check-expect(suprise (list E0 E1 E2   )) (list  E1 E0 E2 ))

(define (suprise los)
  (cond[(empty? (rest los)) (cons (first los) empty) ] 			;String
       [else
         (insert(first los)
          (suprise (rest los))) ]))

;; Email ListOfEmail -> ListOfEmail
;; inserts an email in the correct position of a list of sorted(decending) e-mails

(check-expect(insert  (make-email "di" 2 "wow") (list (make-email "a" 15 "whe") (make-email "a" 14 "wow")  ))(list   (make-email "a" 15 "whe") (make-email "a" 14 "wow") (make-email "di" 2 "wow")))
(define (insert  email los)
  (cond [(empty? los) (cons email empty) ] 			;String
       [else
        (if(> (get-date email) (get-date(first los)))
           (cons email los)
           (cons (first los) (insert email (rest los))))]))

;

(define (get-message e)
   (email-message e))

(check-expect(insertm (make-email "a" 14 "wow") (list (make-email "di" 2 "heat")  (make-email "a" 15 "whe") )) (list (make-email "di" 2 "heat")  (make-email "a" 15 "whe") (make-email "a" 14 "wow")))

(define (insertm  email los)
  (cond [(empty? los) (cons email empty) ] 			;String
       [else
        (if(string<? (get-message email) (get-message(first los)))
           (cons email los)
           (cons (first los) (insertm email (rest los))))]))
        
;;----------------------------

(define-struct gp (name score))
; A GamePlayer is a structure:
; – (make-gp String Number)
; interp. (make-gp p s) represents player p who scored
; a maximum of s points 
#;(define (fn-for-gp gp)
    (...(gp-name)
       (gp-score)))
;problem
;Design a program that sorts lists of game players by score: 


;; ListOfPlayers is one of:
;; - empty
;; - (cons GamePlayer ListOfGamePlayer)
(define (fn-for-los los)
  (cond[(empty? los) (...)] 			;String
       [else
        (... (first los)
        (fn-for-los (rest los)))]))
;------------------------------
;Design a program that sorts lists of game players by score:     


;; GamePlayer -> Number
;; Produces players score
(define (get-score gp)
    (gp-score gp))

;; ListofGamePlayer -> ListOfGamePlayer
;; Produces sorted list of game players
;; Assume at list 1 player

(check-expect(sorted-scores  (list (make-gp "a" 15)  (make-gp "a" 13) (make-gp "a" 17) )) (list (make-gp "a" 17)  (make-gp "a" 15) (make-gp "a" 13) ))

(define (sorted-scores los)
  (cond[(empty? los) empty] 			;String
       [else
        (insertgp (first los)
        (sorted-scores(rest los)))]))

(check-expect(insertgp (make-gp "a" 15) (list  (make-gp "a" 17) (make-gp "a" 13))) (list (make-gp "a" 17)  (make-gp "a" 15) (make-gp "a" 13) ))

(define (insertgp gp logp)
  (cond[(empty? logp) (cons gp empty)] 			;String
       [else
        (if(> (get-score gp) (get-score (first logp)))
           (cons gp logp)
           (cons (first logp)(insertgp gp (rest logp))))]))

;; example and exercise 174
(require 2htdp/image)

(define-struct Posn (x y))

(define MT (empty-scene 50 50))

; A NELoP is one of:
; – (cons Posn empty)
; – (cons Posn NELoP)
 
; NELoP -> Image
; connect the dots in p by rendering lines in MT
;(define (connect-dots p)
;  MT)

(check-expect (connect-dots (list (make-posn 20 0)
                                  (make-posn 10 10)
                                  (make-posn 30 10)))
              (add-line
               (add-line MT 20 0 10 10 "red")
               10 10 30 10 "red"))

(define (connect-dots p)
  (cond
    [(empty? (rest p)) MT ]
    [else
      (render-line
        (connect-dots (rest p))  (first p) (second p)  )]))

(define (render-line im p q)
  (add-line
    im (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red"))

; NELoP -> Posn
; to extract the last Posn on p
(define (last p)
  (cond[(empty? (rest(rest(rest p)))) (third p)]
       [else (last (rest p))]))

; A Polygon is one of:
; – (list Posn Posn Posn)
; – (cons Posn Polygon)
 
;(define MT (empty-scene 50 50))
(check-expect(render-polygon (list (make-posn 20 0)
                                  (make-posn 10 10)
                                  (make-posn 30 10))
             )(add-line
               (add-line
               (add-line MT 20 0 10 10 "red")
               10 10 30 10 "red") 30 10 20 0 "red"))

             ; Polygon -> Image
; add the Polygon p into an image in MT
(define (render-polygon p)
  (render-line (connect-dots p) (first p) (last p)))

(check-expect(render-polygona (list (make-posn 20 0)
                                  (make-posn 10 10)
                                  (make-posn 30 10))
             )(add-line
               (add-line
               (add-line MT 20 0 10 10 "red")
               10 10 30 10 "red") 30 10 20 0 "red"))

             ; Polygon -> Image
; add the Polygon p into an image in MT
(define (render-polygona p)
  (connect-dots (cons (last p) p)))

;exercise 198
;Use contains? to define functions that search for "atom", "basic", and "zoo", respectively.
; String Los -> Boolean
; to determine whether l contains the string s
(define (contains? s l)
  (cond
    [(empty? l) false]
    [else (or (string=? (first l) s)
              (contains? s (rest l)))]))

(check-expect(zoo? '())false)
(check-expect(zoo? '("zoo"))true)
(check-expect(basic? '("a" "b""zoo"))false)
(check-expect(basic? '("a" "basic""zoo"))true)
(check-expect(atom? '("atom" "basic""zoo"))true)
(define (zoo? los)
  (contains? "zoo" los))
(define (basic? los)
  (contains? "basic" los))
(define (atom? los)
  (contains? "atom" los))


;exercise 199
(check-expect(add1* '(1 3 4 5))(list 2 4 5 6))
; Lon -> Lon
; add 1 to each number on l
#;(define (add1* l)
  (cond
    [(empty? l) '()]
    [else
     (cons (add1 (first l))
           (add1* (rest l)))]))

(check-expect(plus5 '(1 3 4 5))(list 6 8 9 10))
; Lon -> Lon
; add 5 to each number on l
#;(define (plus5 l)
  (cond
    [(empty? l) '()]
    [else
     (cons (+ (first l) 5)
           (plus5 (rest l)))]))

; Number LON -> LON
; adds number to list of Numbers
(check-expect(adder 1 (list 1 2 3))(list (+ 1 1) 3 4))
(check-expect(adder 5 (list 1 2 3))(list (+ 1 5) 7 8))
(check-expect(biner + 1 (list 1 2 3))(list (+ 1 1) 3 4))
(check-expect(biner + 5 (list 1 2 3))(list (+ 1 5) 7 8))
(check-expect(biner - 2 (list 4 3 2))(list (- 4 2) 1 0))
;(define (adder num lon)empty)

(define (adder num lon)
  (biner + num lon))

(define (biner comb num lon)
  (cond[(empty? lon) empty]
       [else
        (cons (comb (first lon) num)
              (biner comb num (rest lon)))]))

(define (add1* l) 
  (adder 1 l))

(define (plus5 l) 
  (adder 5 l))

; Number LON -> LON
; adds number to list of Numbers
(check-expect(suber 1 (list 3 2 1 ))(list 2 1 0))
(check-expect(sub2 (list 4 3 2 ))(list 2 1 0))
(check-expect(sub2 (list 4 3 2 ))(list 2 1 0))

(define (suber num lon)
  (biner - num lon))

(define (sub2 lon)
  (suber 2 lon))


; Nelon -> Number
; to determine the smallest
; number on l
(check-expect(extrem < (list 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1))1)
(define (extrem cmp l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (local[(define rester (extrem cmp (rest l))) ]
     (cond
       [(cmp (first l) rester)
        (first l)]
       [else
        rester]))]))

(define (inf lon)
  (extrem < lon))

(define-struct point (hori veri))

(define-struct layer (stuff))
; A Nested-string is one of:
; – Stuff
; – (make-layer Nested-stuff)

(define-struct Nested-string (make-layer String))
; A Nested-string is one of:
; – String
; – (make-layer Nested-string)
(define NS (make-layer "a"))

(define-struct Nested-number (make-layer Number))
; A Nested-number is one of:
; – Number
; – (make-layer Nested-number)
(define NN (make-layer 1))

