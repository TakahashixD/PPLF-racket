#lang racket

(provide 99/length)

(define (99/length a-list)
  (if (empty? a-list)
      0
      (+ 1 (99/length (cdr a-list)))))
