(define initial-interp-env '(lambda (y) ; initial env evaluates omega
                        ((lambda (x) (x x)) (lambda (x) (x x)))))

(define Y '(lambda (f)
            ((lambda (x) (x x))
             (lambda (g) (f (lambda (x) (lambda (y) (((g g) x) y))))))))

(define my-append
  '(lambda (f)
    (lambda (l)
      (lambda (s)
        (if (null? l)
          s
          (cons (car l) ((f (cdr l)) s)))))))


(define (interp-eval expr)
  `(letrec ((eval-expr
              (lambda (expr env)
                (match expr
                  [(? number? n) n]
                  [(? symbol? x) (env x)]
                  [`(lambda (,(? symbol? x)) ,body)
                    (lambda (a)
                      (eval-expr body (lambda (y)
                                        (if (equal? x y)
                                          a
                                          (env y)))))]
                  [`(cons ,a ,d)
                    (cons (eval-expr a env) (eval-expr d env))]
                  [`(car ,l)
                    (car (eval-expr l env))]
                  [`(cdr ,l)
                    (cdr (eval-expr l env))]
                  [`(quote ,v)
                    v]
                  [`(null? ,l)
                    (null? (eval-expr l env))]
                  [`(if ,test
                      ,then
                      ,else)
                    (if (eval-expr test env)
                      (eval-expr then env)
                      (eval-expr else env))]
                  [`(,rator ,rand)
                    ((eval-expr rator env) (eval-expr rand env))]))))
     (eval-expr ,expr
                ,initial-interp-env)))

(display
  (run* (q)
      (evalo
        (interp-eval `'(((,Y ,my-append) (cons 1 '())) '()))
       q)))

(display "\n")



