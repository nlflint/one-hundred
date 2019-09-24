#lang racket
(require rackunit)


;----------------------
; creates a list of operations from Base 3 of given number (like bit flags)
;----------------------

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
; Create all combinations of 8 operations (+ - none)
;--------

(define all-ops-patterns (map
 (lambda (ops-flags)
   (create-ops ops-flags 8)) (range 6560)))

;------------------------------------
;append digit to left
;----------------------

(define (tens-places number)
  (exact-ceiling (/ (log number ) (log 10))))

(check-equal? (tens-places 6) 1)
(check-equal? (tens-places 19) 2)
(check-equal? (tens-places 104) 3)

(define (append-left decimal digit)
  (+ decimal (* digit (expt 10 (tens-places decimal)))))
(check-equal? (append-left 2 8) 82)
(check-equal? (append-left 12 8) 812)
(check-equal? (append-left 123 8) 8123)

;1 to 9 = 10
;10 to 99 = 100
;100 to 999 = 1000

;-------------------------
;-- function to test ops-patterns for 100
;---------------------------
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
          [(eq? operation `none) (equal-one-hundred-impl? (rest ops-pattern) (rest numbers) (append-left working-number digit) current-sum)]))))


(check-equal? (equal-one-hundred? (list + + `none - + `none - +)) #t)

;----------------------------------------------
;--- Count solutions
;-----------------------------

(string-append "Solutions found: " (number->string
                                    (count (lambda (ops-pattern)
                                             (equal-one-hundred? ops-pattern))
                                           all-ops-patterns)))

;-------------------------
;--- A pretty-print function for ops-patterns
;--------------------------------

(define (pretty-print ops-pattern)
  (pretty-print-impl "1" ops-pattern (list 2 3 4 5 6 7 8 9)))

(define (pretty-print-impl string ops-pattern numbers)
  (if (empty? numbers)
      (string-append string " = 100")
      (let ([operation (first ops-pattern)]
            [number (number->string (first numbers))]) 
        (cond
          [(eq? operation +) (pretty-print-impl (string-append string " + " number) (rest ops-pattern) (rest numbers))]
          [(eq? operation -) (pretty-print-impl (string-append string " - " number) (rest ops-pattern) (rest numbers))]
          [(eq? operation `none) (pretty-print-impl (string-append string number) (rest ops-pattern) (rest numbers))]))))

(check-equal? (pretty-print (list + + `none - + `none - +)) "1 + 2 + 34 - 5 + 67 - 8 + 9 = 100")

;-------------------------------
;-- Finally, print all solutions
;-------------------------------

(map pretty-print (filter equal-one-hundred? all-ops-patterns))

