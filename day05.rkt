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

(define (ranges-overlap? range1 range2)
  (or
   (is-in-range? range1 (car range2))
   (is-in-range? range2 (car range1))
   ))

(define (join-ranges range1 range2)
  (cons (min (car range1) (car range2)) (max (cdr range1) (cdr range2))))

; This unfortunately stops as soon as we join the range into the first joinable range.
; Consider the following: We have ranges [1-6] and [7-10], now we try to insert [5-8].
; The result will be [1-8] [7-10]...
; We could change this by starting a new `foldl` operation even if we successfully join the range.
(define (try-join-ranges new-range ranges)
  (cond [(empty? ranges) (list new-range)]
        [(ranges-overlap? (first ranges) new-range)
         (list* (join-ranges (first ranges) new-range) (drop ranges 1))]
        [else (list* (first ranges) (try-join-ranges new-range (drop ranges 1)))]
        ))

(define (join-all-ranges ranges)
  (foldl try-join-ranges '() ranges))

; Instead of fixing the above comment we can also just keep running it until we find a fixpoint.
; The algorithmic complexity will both time be O(n^2)
; But if I had a bit more time I'd add the recursive fold above for nicer code.
(define (join-all-ranges-rec ranges)
  (define prev-l (length ranges))
  (define joint (join-all-ranges ranges))
  (if (= prev-l (length joint))
      joint
      (join-all-ranges-rec joint)))

(define (sum-ranges ranges)
  (for/sum ([r ranges]) (+ (- (cdr r) (car r)) 1)))

(define (solve2 input)
  (define-values (ranges ingredients) (parse input))
  (define joint-ranges (join-all-ranges-rec ranges))
  (sum-ranges joint-ranges))

(printf "solution2: ~a\n" (solve2 input))