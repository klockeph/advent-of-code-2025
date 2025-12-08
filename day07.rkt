#lang racket

(require "common.rkt")

(define input (read-input-lines "inputs\\day07.txt"))

(define (parse-input input)
  (define start-pos (void))
  (define splitters (mutable-set))
  (define height (void))
  (define (handler coords char)
    (cond [(char=? char #\S) (set! start-pos coords)]
          [(char=? char #\^) (set-add! splitters coords)]
          [else (set! height (cdr coords))]))
  (parse-grid input handler)
  (values start-pos splitters height))

(define (b-split b)
  (values (cons (- (car b) 1) (cdr b))
          (cons (+ (car b) 1) (cdr b))))

(define (dict-update-add! dict k val)
  (cond [(dict-has-key? dict k)
         (define (add x) (+ x val))
         (dict-update! dict k add)]
        [else (dict-set! dict k val)]))

;; particles is a dict of pos -> num particles
;; splitters is a set of pos
;; returns (new-particles, num-splits-occured)
(define (beam-down particles splitters)
  (for/fold ([new-parts (make-hash)]
             [splits 0])
            ([(b num-p) particles])
    (define down-b (cons (car b) (+ 1 (cdr b))))
    (cond [(set-member? splitters down-b)
           (define-values (l r) (b-split down-b))
           (dict-update-add! new-parts l num-p)
           (dict-update-add! new-parts r num-p)
           (values new-parts (+ splits 1))]
          [else
           (dict-update-add! new-parts down-b num-p)
           (values new-parts splits)])))

;; track all the particles to the end: remember how many splits occurred
;; and where the resulting particles end up (and how many they are)
(define (track particles splitters max-height (acc 0))
  (cond [(dict-empty? particles) (cons (void) acc)]
        [(= 0 max-height) (cons particles acc)]
        [else
         (define-values (new-parts new-splits) (beam-down particles splitters))
         (track new-parts splitters (- max-height 1) (+ acc new-splits))]))

(define (get-track input)
  (define-values (start-pos splitters height) (parse-input input))
  (define start-particle (make-hash))
  (dict-set! start-particle start-pos 1)
  (track start-particle splitters height))

(define solution1
  (cdr (get-track input)))
(printf "solution1: ~a\n" solution1)

(define solution2
  (apply + (dict-values (car (get-track input)))))
(printf "solution2: ~a\n" solution2)