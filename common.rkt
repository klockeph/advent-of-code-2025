#lang racket

(define (read-input-lines filename)
  (with-input-from-file filename
    (lambda ()
      (port->lines (current-input-port)))))
(provide read-input-lines)