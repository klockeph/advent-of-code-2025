#lang racket 

(require "common.rkt")

(define input (read-input-lines "inputs\\day03.txt"))

(define (max-num digits)
  (define digits-length (length digits))
  (define first-digit-pos (argmax-index identity (take digits (- digits-length 1))))
  (define second-digit-pos (+ (argmax-index identity (drop digits (+ first-digit-pos 1))) first-digit-pos 1))
  (define first-digit (list-ref digits first-digit-pos))
  (define second-digit (list-ref digits second-digit-pos))
  (+ (* 10 first-digit) second-digit)
   )

(define (digit-value digit-char)
  (- (char->integer digit-char)
     (char->integer #\0)))

(define (max-joltage line)
  (max-num (map digit-value (string->list line)))
   )

(define solution1
  (for/sum ([l input]) (max-joltage l)))

(printf "solution1: ~a\n" solution1)

(define t (map max-joltage input))
