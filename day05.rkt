#lang racket

(require racket/list)

(require "common.rkt")
(define input (read-input-lines "inputs\\day05.txt"))

(define (parse-range range)
  (define l (map string->number (string-split range "-")))
  (cons (first l) (second l)))

(define (string-empty? s) (string=? "" s))
(define (string-not-empty? s) (not (string-empty? s)))

(define (parse input)
  (define-values (ranges ingredients) (splitf-at input string-not-empty?))
  (values (map parse-range ranges) (map string->number (cdr ingredients))))

(define (is-in-range? range id)
  (and (<= (car range) id) (>= (cdr range) id)))

(define (is-in-any-range? ranges id)
  (for/or ([range ranges]) (is-in-range? range id)))

(define (solve1 input)
  (define-values (ranges ingredients) (parse input))
  (define (range-check id) (is-in-any-range? ranges id))
  (length (filter range-check ingredients)))

(printf "solution1: ~a\n" (solve1 input))
