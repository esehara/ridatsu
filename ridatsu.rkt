#lang racket

(define (template-repeat x name)
  (string-append x " evalto " x " by " name " {};"))

(define (evalto x env)
  (cond [(not (or (list? x)))   (raise "Evalto Error: Expected List" #f)]
        [(not (or (list? env))) (raise "Evalto Error: Enviroment must be List" #f)]
        [else
         (string-append 
         (let ([head (car x)]
              [body (cdr x)])
          (cond
            [(number? head) (template-repeat (number->string head) "E-Int")]
            [(boolean? head) (template-repeat (if head "true" "false") "E-Bool")]))]))

(module+ test
  (require rackunit)
  
  (check-equal? (evalto (list 1)  '()) "1 evalto 1 by E-Int {};")
  (check-equal? (evalto (list #t) '()) "true evalto true by E-Bool {};")
  (check-equal? (evalto (list #f) '()) "false evalto false by E-Bool {};")
  )