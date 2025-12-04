#lang racket

(require racket/set)
(require "common.rkt")

(define input (read-input-lines "inputs\\day04.txt"))

(define (parse-input input)
  (define h (length input))
  (define w (string-length (first input)))
  (define s (mutable-set))
  (for ([line input] [y (range h)])
    (for ([char line]
          [x (range w)]
          #:when (char=? char #\@))
      (set-add! s (cons x y))
      ))
  s)

(define (get-neighbors p)
  (for*/list ([dx '(-1 0 1)]
              [dy '(-1 0 1)]
              #:unless (and (= 0 dx) (= 0 dy)))
    (cons (+ dx (car p)) (+ dy (cdr p)))
    )
  )

(define (get-valid-neighbors p s)
  (define n (get-neighbors p))
  (define (f x) (set-member? s x))
  (filter f n))

(define (can-move p s)
  (< (length (get-valid-neighbors p s)) 4))

(define (get-movables s)
  (filter (lambda (p) (can-move p s)) (set->list s)))

(define solution1
  (length (get-movables (parse-input input))))

(define (remove-movables-rec s (acc 0))
  (define to-remove (get-movables s))
  (cond
    [(empty? to-remove) acc]
    [else
       (for ([m to-remove]) (set-remove! s m))
       (remove-movables-rec s (+ acc (length to-remove)))]))

(define solution2
  (remove-movables-rec (parse-input input)))

(printf "solution1: ~a\n" solution1)
(printf "solution2: ~a\n" solution2)