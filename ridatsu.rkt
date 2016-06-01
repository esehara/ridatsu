#lang racket

(define (any->string x)
  (cond
    [(symbol? x) (symbol->string x)]
    [(number? x) (number->string x)]
    [(boolean? x) (if x "true" "false")]
    [(string? x) x]
    [(list? x) (list->exp x)]
    [else (raise "Do not know string method")]))

(define (list->exp x)
    (cond [(symbol=? 'let (first x))
           (let* ([exp (any->string (first x))]
                  [name (any->string (second x))]
                  [e1 (any->string (third x))]
                  [e2 (any->string (fourth x))])
             (string-append "(" exp name " = " e1 " in " e2 ")" ))]
          [(symbol=? 'fun (first x))
           (let* ([exp (any->string (first x))]
                  [name (any->string (second x))]
                  [e1 (any->string (third x))])
             (string-append "(" exp " " name " -> " e1 ")"))]
          [(= 2 (length x))
           (let ([e1 (any->string (first x))]
                 [e2 (any->string (second x))])
             (string-append "(" e1 " " e2 ")" ))]
          [else
           (let* ([exp (any->string (first x))]
                  [e1 (any->string (second x))]
                  [e2 (any->string (third x))])
             (string-append "(" e1 " " exp " " e2 ")" ))]))

(define (template-repeat x name)
  (let ([str-x (any->string x)])
    (string-append str-x " evalto " str-x " by " name " {};")))

(define (localenv->string env)
  (if (null? env) ""
       (string-join
        (map (lambda (x) (string-append (car x) " = " (any->string (cadr x)))) (reverse env))
        ", ")))

(define (env->string env)
  (string-append (localenv->string env) " |- "))

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
    [(string? x) (valueto (cadr
                           (car (filter (lambda (y) (string=? x (car y))) env))) env)]
    [(list? x)
     (if (symbol=? 'fun (car x)) x
      (let ([head (car x)]
            [body (map (lambda (y) (valueto y env)) (cdr x))])
        (cond
          [(list? head) (valueto head)]
          [(symbol=? '+ head) (apply + body)]
          [(symbol=? '- head) (apply - body)]
          [(symbol=? '* head) (apply * body)]
          [else (valueto-userdef x env)])))]
    [else x]))

(define (valueto-userdef x env)
  (let*([name (any->string (first x))]
        [param (second x)]
        [find-function (userdef-function-from-env name env)])
  (if (null? find-function) (raise "Do not know value")
      (let* ([userdef (cadr (car find-function))]
             [name   (second userdef)]
             [body    (third  userdef)])
        (valueto body (cons (list name param) env))))))

(define (template-e1e2 e1 e2 f op bop name env)
  (string-append (any->string e1) op (any->string e2) " evalto "
                           (any->string (f (valueto e1 env) (valueto e2 env))) " by E-" name " {\n"
                           "" (evalto (list e1) env) "\n"
                           "" (evalto (list e2) env) "\n"
                           "" (any->string (valueto e1 env)) " " bop " " (any->string (valueto e2 env)) " is "
                           (any->string (f (valueto e1 env) (valueto e2 env)))
                           " by B-" name " {};\n"
                           "};\n"))

(define (evalto-function-template x env f op bop name)
  (if (not (= 3 (length x))) (raise "Apply Error: Wrong argument numeber.")
      (let ([e1 (second x)]
            [e2 (third  x)])
            (template-e1e2 e1 e2 f op bop name env))))

(define (evalto-plus x env)  (evalto-function-template x env + " + " "plus" "Plus"))
(define (evalto-minus x env) (evalto-function-template x env - " - " "minus" "Minus"))
(define (evalto-times x env) (evalto-function-template x env * " * " "times" "Times"))
(define (evalto-lt x env)    (evalto-function-template x env < " < " "less than" "Lt"))

(define (evalto-if x env)
  (if (not (= 4 (length x))) (raise "Apply Error If: Wrong argument number.")
      (let ([e1 (second x)]
            [e2 (third  x)]
            [e3 (fourth  x)])
        (string-append " if " (any->string e1) " then " (any->string e2) " else " (any->string e3)
                       " evalto "
                       (if (valueto e1 env) (any->string (valueto e2 env)) (any->string (valueto e3 env)))
                       " by "
                       (if (valueto e1 env) "E-IfT" "E-IfF") "{\n"
                       "" (evalto (list e1) env) "\n"
                       "" (if (valueto e1 env) (evalto (list e2) env) (evalto (list e3) env))
                       "\n};"
                       ))))

(define (evalto-let x env)
  (if (not (= 4 (length x))) (raise "Apply Error Let: Wrong argument number.")
           (let ([name (second x)]
                 [e1 (third  x)]
                 [e2 (fourth x)])
             (string-append " let " name " = " (any->string e1) " in " (any->string e2) " by E-Let {\n"
                            (evalto e1 env) "\n"
                            (evalto e2 (cons (list name (valueto e1 env)) env)) "\n"
                            "};\n"))))

(define (evalto-fun x env)
  (let ([name (second x)]
        [e1   (any->string(third  x))])
    (string-append "fun " name " -> " e1 " evalto "
                   "(" (localenv->string env) ")[fun " name " -> " e1 "] by E-Fun {};")))

(define symbol->function
  (list (list '+ evalto-plus)
        (list '- evalto-minus)
        (list '* evalto-times)
        (list '< evalto-lt)
        (list 'if evalto-if)
        (list 'let evalto-let)
        (list 'fun evalto-fun)))

(define (userdef-function-from-env name env)
  (filter (lambda (y) (and (string=? name (car y))
                           (symbol=? 'fun (caar (cdr y))))) env))

(define (evalto-find-function x env)
  (let* ([rule (symbol->string (car x))]
         [param (second x)]
         [find-function-from-env (userdef-function-from-env rule env)])
    (if (null? find-function-from-env) (raise "Do not find function.")               
        (let* ([user-def (car (cdr (car find-function-from-env)))]
               [name (second user-def)]
               [body (third user-def)])
          (string-append
           rule " " (any->string param) " by E-App {\n"
           (evalto user-def env) "\n"
           (evalto (list param) env) "\n"
           (evalto body (cons (list name param) env)) "\n"
            "\n"
           "};\n"
           )))))

(define (evalto-symbol x env)
  (let* ([rule (car x)]
         [find-function (filter (lambda (y) (symbol=? rule (car y))) symbol->function)])
    (if (null? find-function) (evalto-find-function x env)
        (apply (cadr (car find-function))
               (list x env)))))

(define (evalto x env)
  (cond [(not (list? x))   (raise "Evalto Error: Expected List" #f)]
        [(not (list? env)) (raise "Evalto Error: Enviroment must be List" #f)]
        [else
         (let ([head (car x)]
               [body (cdr x)])
           (if (list? head)
               (if (symbol=? 'fun (first head))
                   (evalto-fun x env)
                   (evalto head env))
           (string-append
            (env->string env)
            (cond
              [(number? head) (template-repeat head "E-Int")]
              [(boolean? head) (template-repeat head "E-Bool")]
              [(symbol? head) (evalto-symbol x env)]
              [(string? head) (evalto-var head env)]
              [else (begin (write head "\n")
                           (raise "Evalto Error: Do not understand syntax."))]
             ))))]))

(define (list->env lst) (reverse lst))

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