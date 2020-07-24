#lang racket

(define (sum a b)
  (+ a b))

(/ 1 1)

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define funny-number 69)

"This program defines factorial, sum, and funny-number"