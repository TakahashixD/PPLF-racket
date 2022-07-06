#lang racket

(provide 99/last)

(define (99/last a-list)
  (let ([rest-list (cdr a-list)])
    (if (empty? rest-list)
        (car a-list)
        (99/last rest-list))))