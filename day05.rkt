#lang racket

(require "common.rkt")
(define input (read-input-lines "inputs\\day05.txt"))

(define (parse-range range)
  (define-values (starts ends) (split-at input #\-))
  (cons (string->number starts) (string->number ends)))
  

(define (parse input)
  (define-values (ranges ingredients) (splitf-at input (compose1 string=? "" -)))
  values (map parse-range ranges) (map string->number ingredients))
