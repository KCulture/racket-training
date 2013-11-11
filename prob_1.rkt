;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname prob_1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(define (sum-3-5 n)
  (local[(define (3or5 n)
  (if(or (integer?(/ n 5))
      (integer?(/ n 3)))
     n
     0))]

  (foldr + 0 (map 3or5 (build-list n identity)))))