#lang racket

(require "common.rkt")

(define input (string-split (first (read-input-lines "inputs\\day02.txt")) ","))

(define (get-range str)
  (define l (string-split str "-"))
  (define a (string->number (first l)))
  (define b (string->number (second l)))
  (range a (+ b 1)))

(define (repeating-pattern? pattern whole-string)
  (define p-length (string-length pattern))
  (define s-length (string-length whole-string))
  (if (not (= (modulo s-length p-length) 0)) #f
      (for/and ([i (range 1 (/ s-length p-length))])
        (string=? (substring whole-string (* i p-length) (* (+ i 1) p-length)) pattern)
        )
      ))

(define (sum-invalids-curried check-fn)
  (lambda (range-string)
    (define range (get-range range-string))
    (for/sum ([id-number range]
              #:when (check-fn id-number))
      id-number
      )
    )
  )

(define (is-invalid-1 id-number)
  (define s (number->string id-number))
  (define len (string-length s))
  (and (= (modulo len 2) 0) (repeating-pattern? (substring s 0 (quotient len 2)) s)))

(define (solve invalid-check)
  (for/sum ([i input]) ((sum-invalids-curried invalid-check) i))
  )

(define solution1 (solve is-invalid-1))
(printf "solution1: ~a\n" solution1)


(define (invalid-2? id-number)
  (define s (number->string id-number))
  (for/or ([p-len (range 1 (string-length s))])
    (let ([pattern (substring s 0 p-len)])
      (repeating-pattern? pattern s))))

(define solution2 (solve invalid-2?))
(printf "solution2: ~a\n" solution2)