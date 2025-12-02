#lang racket

(require "common.rkt")

(define test-input '("11-22"
                     "95-115"
                     "998-1012"
                     "1188511880-1188511890"
                     "222220-222224"
                     "1698522-1698528"
                     "446443-446449"
                     "38593856-38593862"
                     "565653-565659"
                     "824824821-824824827"
                     "2121212118-2121212124"
                     ))


;;(define input test-input)

(define input (string-split (first (read-input-lines "inputs\\day02.txt")) ","))

(define (get-range str)
  (define l (string-split str "-"))
  (define a (string->number (first l)))
  (define b (string->number (second l)))
  (range a (+ b 1)))

(define (is-invalid id-number)
  (define s (number->string id-number))
  (define len (string-length s))
  (if (= (modulo len 2) 1) #f
      (let ([h (/ len 2)])
        (let ([l (substring s 0 h)])
          (let ([r (substring s h len)])
            (string=? l r))))
      )
  )

(define (sum-invalids range-string acc)
  (define range (get-range range-string))
  (+ acc (for/sum ([id-number range]
                   #:when (is-invalid id-number))
           id-number
           ))
  )

(define solution1
  (foldl sum-invalids 0 input)
  )

(display solution1)