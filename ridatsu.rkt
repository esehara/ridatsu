#lang racket

(define (any->string x)
  (cond
    [(symbol? x) (symbol->string x)]
    [(number? x) (number->string x)]
    [(boolean? x) (if x "true" "false")]
    [else x]))

(define (template-repeat x name)
  (let ([str-x (any->string x)])
    (string-append str-x " evalto " str-x " by " name " {};")))

(define (env->string env)
  (string-append
   (if (null? env) ""
       (string-join
        (map (lambda (x) (string-append (car x) " = " (any->string (cadr x)))) (reverse env))
        ", ")) " |- "))

(define (evalto-var x env)
  (cond [(null? env) (raise "Not found value error" #f)]
        [(string=? x (caar env))
         (string-append x
                        " evalto " (any->string (cadr (car env))) " by E-Var1 {};")]
        [else
         (string-append x
                        " evalto " (any->string (cadr (car env))) " by E-Var2 {\n"
                        "  " (evalto (list x) (cdr env))
                        "\n}")]))

(define (valueto x env)
  (cond
    [(number? x) x]
    [(string? x) (valueto (cadr (car (filter (lambda (y) (string=? x (car y))) env))) env)]
    [else x]))

(define (evalto-plus x env) "TODO"
  (if (not (= 3 (length x))) (raise "Apply plus Error: Wrong argument numeber.")
      (let ([e1 (second x)]
            [e2 (third  x)])
            (string-append (any->string e1) " + " (any->string e2) " evalto "
                           (any->string (+ (valueto e1 env) (valueto e2 env))) " by E-Plus {/n"
                           ))))

(define (evalto-symbol x env)
  (let ([rule (car x)])
  (cond [(symbol=? rule 'plus) (evalto-plus x env)])))
         
(define (evalto x env)
  (cond [(not (list? x))   (raise "Evalto Error: Expected List" #f)]
        [(not (list? env)) (raise "Evalto Error: Enviroment must be List" #f)]
        [else
         (string-append
          (env->string env)
          (let ([head (car x)]
                [body (cdr x)])
            (cond
              [(number? head) (template-repeat head "E-Int")]
              [(boolean? head) (template-repeat head "E-Bool")]
              [(symbol? head) (evalto-symbol x env)]
              [(string? head) (evalto-var head env)]
              )))]))

(module+ test
  (require rackunit)
  ;; Enviroment convate String
  (check-equal? (env->string '()) " |- ")
  (check-equal? (env->string (list '("x" 1))) "x = 1 |- ")

  ;; Rules
  (check-equal? (evalto (list 1)  '()) " |- 1 evalto 1 by E-Int {};")
  (check-equal? (evalto (list #t) '()) " |- true evalto true by E-Bool {};")
  (check-equal? (evalto (list #f) '()) " |- false evalto false by E-Bool {};")
  (check-equal? (evalto (list "x") (list '("x" 1))) "x = 1 |- x evalto 1 by E-Var1 {};"))