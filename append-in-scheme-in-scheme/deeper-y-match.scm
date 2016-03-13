(define initial-interp-env '(lambda (y) ; initial env evaluates omega
                        ((lambda (x) (x x)) (lambda (x) (x x)))))

(define Y '(lambda (f)
            ((lambda (x) (x x))
             (lambda (g) (f (lambda (x) (lambda (y) (((g g) x) y))))))))

(define my-append
  '(lambda (f)
    (lambda (l)
      (lambda (s)
        (match l
          ['() s]
          [(cons a d) (cons a ((f d) s))])))))


(define (interp-eval expr)
  `(letrec ((eval-expr
              (lambda (expr env)
                (match expr
                  [(? number? n) n]
                  [(? symbol? x) (env x)]
                  [`(quote ,v) v]
                  [`(lambda (,(? symbol? x)) ,body)
                    (lambda (a)
                      (eval-expr body (lambda (y)
                                        (if (equal? x y)
                                          a
                                          (env y)))))]
                  [`(match ,e1
                      ['() ,e2]
                      [(cons ,v1 ,v2) ,e3])
                    ((lambda (val)
                       (if (null? val)
                         (eval-expr e2 env)
                         (eval-expr e3 (lambda (y)
                                         (if (equal? y v1)
                                           (car val)
                                           (if (equal? y v2)
                                             (cdr val)
                                             (env y)))))))
                     (eval-expr e1 env))]
                  [`(,rator ,rand)
                    ((eval-expr rator env) (eval-expr rand env))]))))
     (eval-expr ,expr
                ,initial-interp-env)))



(display
  (run* (q)
      (evalo
        (interp-eval `'(((,Y ,my-append) '(1 2)) '(3 4)))
       q)))

(display "\n")



