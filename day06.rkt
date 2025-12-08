#lang racket

(require "common.rkt")

(define input (read-input-lines "inputs\\day06.txt"))

(define (string->obj s)
  (cond
    [(string=? s "+") +]
    [(string=? s "*") *]
    [else (string->number s)]))

(define (line->list-obj l)
  (for/list [(o (string-split l " "))
             #:unless (string=? o "")]
    (string->obj o)))

(define (solve1 input)
  (define ls (map line->list-obj input))
  (define-values (nums ops) (split-at-right ls 1))
  (define transposed (apply map list nums))
  (for/sum [(op (car ops))
            (vals transposed)]
    (apply op vals))
  )

(printf "solution1: ~a\n" (solve1 input))

;; Parses a column of e.g. '(#\1 #\2) to 12
;; Spaces are ignored as long as there's also digits: '(#\7 #\6 #\space) -> 76
;; A column of only spaces '(#\space #\space) becomes #<void> 
(define (try-parse col)
  (cond [(for/and [(c col)] (char-whitespace? c)) (void)]
        [else (string->number (string-trim (list->string col)))]))

;; Couldn't find a good name for this function.
;; Essentially it takes a list of parsed-ops (#<procedure +> #<procedure *> ...)
;; And a list of #void-separated nums (1 2 3 #<void> 4 13)
;; Then it performs the ops on the correct nums and accumulates: (+ (+ 1 2 3) (* 4 13))
(define (perform-math ops nums (acc 0))
  (cond [(empty? ops) acc]
        [else (define-values (op next-ops) (split-at ops 1))
              (define-values (cur-nums next-nums) (splitf-at nums (lambda (x) (not (void? x)))))
              (define next-acc (+ (apply (car op) cur-nums) acc))
              (cond [(empty? next-nums) next-acc]
                    [else 
                     (perform-math next-ops (cdr next-nums) next-acc)
                     ])]))
  

(define (solve2 input)
  (define-values (numlines op-string) (split-at-right input 1))
  (define ops (line->list-obj (first op-string)))
  (define charlines (map string->list numlines))
  (define charcols (apply map list charlines))
  (define parsed-nums (map try-parse charcols))
  (perform-math ops parsed-nums))

(printf "solution2: ~a\n" (solve2 input))