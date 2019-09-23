#lang racket
(require rackunit)



(check-equal? (remainder 8 3) 2)

(define (create-ops ops-flags digit-count)
  (if (zero? digit-count)
      null
      (let ([end-digit (remainder ops-flags 3)])
        (cons
         (cond
           [(eq? end-digit 0) `none]
           [(eq? end-digit 1) +]
           [(eq? end-digit 2) -])
         (create-ops (quotient ops-flags 3) (- digit-count 1))))))


(check-equal? (create-ops 0 1) (list `none))
(check-equal? (create-ops 1 1) (list +))
(check-equal? (create-ops 2 1) (list -))
(check-equal? (create-ops 6560 8) (list - - - - - - - -))

;----------------------------------------

(define all-ops-patterns (map
 (lambda (ops-flags)
   (create-ops ops-flags 8)) (range 6560)))

;------------------------------------

(define (equal-one-hundred? ops-pattern)
  (equal-one-hundred-impl? (reverse ops-pattern) (list 8 7 6 5 4 3 2 1) 9 100))

   
(define (equal-one-hundred-impl? ops-pattern numbers working-number current-sum)
  (if (empty? ops-pattern)
      (eq? (- current-sum working-number) 0)
      (let ([operation (first ops-pattern)]
            [digit (first numbers)])
        (cond
          [(eq? operation +)(equal-one-hundred-impl? (rest ops-pattern) (rest numbers) digit (- current-sum working-number))]
          [(eq? operation -)(equal-one-hundred-impl? (rest ops-pattern) (rest numbers) digit (+ current-sum working-number))]
          [(eq? operation `none) (equal-one-hundred-impl? (rest ops-pattern) (rest numbers) (+ working-number (* digit 10)) current-sum)]))))


(check-equal? (equal-one-hundred? (list + + `none - + `none - +)) #t)

;----------------------------------------------
;--- Count solutions
;-----------------------------

(count (lambda (ops-pattern)
         (equal-one-hundred? ops-pattern))
       all-ops-patterns)

;-------------------------
;--- Pretty print
;--------------------------------

(define (pretty-print pattern)
  (pretty-print-impl "1" pattern (list 2 3 4 5 6 7 8 9)))

(define (pretty-print-impl string pattern numbers)
  (if (empty? numbers)
      (string-append string " = 100")
      (let ([operation (first pattern)]
            [number (number->string (first numbers))]) 
        (cond
          [(eq? operation +) (pretty-print-impl (string-append string " + " number) (rest pattern) (rest numbers))]
          [(eq? operation -) (pretty-print-impl (string-append string " - " number) (rest pattern) (rest numbers))]
          [(eq? operation `none) (pretty-print-impl (string-append string number) (rest pattern) (rest numbers))]))))

(check-equal? (pretty-print (create-ops 3112 8)) "1 + 2 + 34 - 5 + 67 - 8 + 9 = 100")

;-------------------------------
;-- Finally, print all solutions
;-------------------------------

(map pretty-print (filter equal-one-hundred? all-ops-patterns))
(filter equal-one-hundred? all-ops-patterns)


(equal-one-hundred? (list + + `none `none - `none + `none))

