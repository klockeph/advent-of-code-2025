#lang racket 

(require "common.rkt")

(define input (read-input-lines "inputs\\day03.txt"))

(define (digit-add num d) (+ (* 10 num) d))

(define (max-num count digits [acc 0])
  (cond
    [(= count 1) (digit-add acc (foldl max 0 digits))]
    [else
     (define digits-length (length digits))
     (define curr-digit-i (argmax-index identity (take digits (- digits-length (- count 1)))))
     (max-num (- count 1) (drop digits (+ (cdr curr-digit-i) 1)) (digit-add acc (car curr-digit-i)))]))

(define (digit-value digit-char)
  (- (char->integer digit-char)
     (char->integer #\0)))

(define (max-joltage line count)
  (max-num count (map digit-value (string->list line)))
  )

(define (solve count)
  (for/sum ([l input]) (max-joltage l count)))

(printf "solution1: ~a\n" (solve 2))
(printf "solution2: ~a\n" (solve 12))
