#lang racket
(require racket/pretty)
(require rackunit)
(require rackunit/text-ui)

;;Acha o número de comentarios
(define n 0)
(define (comentario arq n)
  (cond [(null? arq) n]
        [(string-contains? (first arq) ";")
         (comentario (rest arq) (+ n 1))]
        [else
         (comentario (rest arq) n)]))


;;Acha o número de funções
(define m 0)
(define (defs arq m)
  (cond [(null? arq) m]
        [(string-prefix? (first arq) "(define (")
         (defs (rest arq) (+ m 1))]
        [else
         (defs (rest arq)  m)]))

;;Acha o numero de testes
(define (test arq n)
  (cond [(null? arq) n]
        [(string-contains? (first arq) "(check-equal? ")
         (test (rest arq) (+ n 1))]
        [else
         (test (rest arq) n)]))

;;Avaliador de codigo n1 = tamanho, n2 = comentarios, n3 = funções e n4 = testes
(define (gerar-nota n1 n2 n3 n4)
 (+ (* n1 0.2) (* n2 0.1) (* n3 0.3) (* n4 0.2)))

;;Lista de resultados
(define (lista-resultados arq)
  (display "\nCodigo: \n")
  (pretty-display arq)
  (display "\nMetricas: \n")
  (pretty-display (list (string-append "Numero de linhas: " (number->string (length arq)))
        (string-append "\n Numero de comentarios: "(number->string (comentario arq n)))
        (string-append "\n Numero de funcoes: " (number->string (defs arq m)))
        (string-append "\n Numero de testes: "(number->string (test arq n)))))
   (display "\nNota: ")
   (display (gerar-nota (length arq) (comentario arq n) (defs arq m) (test arq n)))
   (display "\n"))

;;Lista com todos os arquivos
(define todos-arquivos(map path->string (directory-list "codigos_racket/" #:build? #t)))
  
;;Ler arquivos e fazer a avaliação
(define (leitura todos-arquivos)
  (cond [(null? todos-arquivos)]
        [(lista-resultados(file->lines(first todos-arquivos)))
         (leitura (rest todos-arquivos))]))

;;Coloca cada linha do arquivo em uma posição de uma lista
(define arquivo1 (file->lines "codigos_racket/Q4-1.rkt"))
(define arquivo2 (file->lines "codigos_racket/Q26-1.rkt"))
(define arquivo3 (file->lines "codigos_racket/Q26 - Combination.rkt"))
;;Testes
(define testes
  (test-suite "testes"
              (check-equal? (length arquivo1) 8)
              (check-equal? (comentario arquivo1 n) 0)
              (check-equal? (defs arquivo1 m) 1)
              (check-equal? (test arquivo1 n) 0)
              (check-equal? (gerar-nota (length arquivo1) (comentario arquivo1 n) (defs arquivo1 m) (test arquivo1 n))1.9000000000000001)
              (check-equal? (length arquivo2) 56)
              (check-equal? (comentario arquivo2 n) 10)
              (check-equal? (defs arquivo2 m) 2)
              (check-equal? (test arquivo2 n) 8)
              (check-equal? (gerar-nota (length arquivo2) (comentario arquivo2 n) (defs arquivo2 m) (test arquivo2 n))14.4)
              (check-equal? (length arquivo3) 21)
              (check-equal? (comentario arquivo3 n) 10)
              (check-equal? (defs arquivo3 m) 1)
              (check-equal? (test arquivo3 n) 0)
              (check-equal? (gerar-nota (length arquivo3) (comentario arquivo3 n) (defs arquivo3 m) (test arquivo3 n))5.5)
))

(define (et t)
  (run-tests (test-suite "Todos os testes" t))
  (void))
