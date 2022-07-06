;; P26 (**) Generate the combinations of K distinct objects chosen
;; from the N elements of a list

;; In how many ways can a committee of 3 be chosen from a group of 12
;; people? We all know that there are C(12,3) = 220 possibilities
;; (C(N,K) denotes the well-known binomial coefficients). For pure
;; mathematicians, this result may be great. But we want to really
;; generate all the possibilities in a list.

;; Example:
;; * (combination 3 '(a b c d e f))
;; ((A B C) (A B D) (A B E) ... )

(define (combination-1 r lista)
  (let ([n (length lista)])
    (cond [(> r n) '()]
          [(zero? r) '(())]
          [else
           (append (map (lambda (com)
                          (cons (car lista) com))
                        (combination-1 (sub1 r) (cdr lista)))
                   (combination-1 r (cdr lista)))])))

(define (combination-2 r lista)
  (define (recur r remain result)
    (let ([n (length remain)])
      (cond [(> r n) '()]
            [(zero? r) (list result)]
            [else
             (append (recur (sub1 r)
                            (cdr remain)
                            (cons (car remain) result))
                     (recur r (cdr remain) result))])))
  (recur r lista '()))

(define combination combination-2)

(check-equal? (combination-1 3 '(1 2 3 4))
              '((1 2 3) (1 2 4) (1 3 4) (2 3 4)))
(check-equal? (combination-2 3 '(1 2 3 4))
              '((3 2 1) (4 2 1) (4 3 1) (4 3 2)))

(check-equal? (combination-1 1 '(1 2 3))
              '((1) (2) (3)))
(check-equal? (combination-2 1 '(1 2 3))
              '((1) (2) (3)))

(check-equal? (combination-1 1 '())
              '())
(check-equal? (combination-2 1 '())
              '())

(check-equal? (combination-1 0 '())
              '(()))
(check-equal? (combination-2 0 '())
              '(()))