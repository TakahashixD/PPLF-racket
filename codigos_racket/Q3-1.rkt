#lang racket

(provide 99/nth)

(define (99/nth a-list n)
  (if (equal? n 1)
      (first a-list)
      (99/nth (rest a-list) (- n 1))))
