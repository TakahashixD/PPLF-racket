#lang racket

(provide 99/last-two)

(define (99/last-two a-list)
  (let* ([rest-list-1 (cdr a-list)]
         [rest-list-2 (cdr rest-list-1)])
    (if (empty? rest-list-2)
        a-list
        (let ([rest-list-3 (cdr rest-list-2)])
          (if (empty? rest-list-3)
              rest-list-1
              (99/last-two rest-list-1))))))
