#lang racket

(require "common.rkt")

(define input-data (read-input-lines "inputs\\day01.txt"))

(define (get-steps instruction)
  (string->number (substring instruction 1)))

(define (turn instruction wheel)
  ((if (char=? (string-ref instruction 0) #\R) + -)
   wheel
   (get-steps instruction)
   ))

(define (step1 instruction acc-wheel)
  (define acc (car acc-wheel))
  (define wheel (cdr acc-wheel))
  (define next (modulo (turn instruction wheel) 100))
  (cons (if (= next 0) (+ acc 1) acc) next)
  )

(define solution1
  (car (foldl step1 '(0 . 50) input-data)))
(printf "solution1: ~a\n" solution1)

;; Returns '(new-wheel . crossed-0)
(define (turn-and-check instruction wheel-pos)
  (define turns (string->number (substring instruction 1)))
  (define right? (char=? (string-ref instruction 0) #\R))
  (define full-turns (quotient turns 100))
  (define real-step (modulo turns 100))
  (define next-wheel-pos ((if right? + -) wheel-pos real-step))
  (define norm-next-wheel-pos (modulo next-wheel-pos 100))
  (define past-zero (or (= norm-next-wheel-pos 0) (not (or (= wheel-pos 0) (= norm-next-wheel-pos next-wheel-pos)))))
  (cons norm-next-wheel-pos (+ full-turns (if past-zero 1 0)))
  )

(define (step2 instruction acc-wheel)
  (define val-crossed (turn-and-check instruction (cdr acc-wheel)))
  (cons (+ (car acc-wheel) (cdr val-crossed)) (car val-crossed))
  )

(define solution2
  (car (foldl step2 '(0 . 50) input-data)))
(printf "solution2: ~a\n" solution2)