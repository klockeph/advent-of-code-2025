#lang racket

(define (read-input-lines filename)
  (with-input-from-file filename
    (lambda ()
      (port->lines (current-input-port)))))
(provide read-input-lines)

(define (argmax-index f lst)
  (define-values (bi mv _ci)
    (for/fold ([best-i 0]
               [max-val (f (first lst))]
               [curr-i 0]
               )
              ([elem (in-list lst)])
      (let ([current-val (f elem)])
        (if (> current-val max-val)
            (values curr-i current-val (+ curr-i 1))
            (values best-i max-val (+ curr-i 1)))))
    )
  (cons mv bi))
(provide argmax-index)
