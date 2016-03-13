(define initial-interp-env '(lambda (y) ; initial env evaluates omega
                        ((lambda (x) (x x)) (lambda (x) (x x)))))


(define (interp-eval expr)
  `(letrec ((ext-env (lambda (key val env)
                       (lambda (y)
                         (if (equal? key y)
                           val
                           (env y))))))
     (letrec ((eval-expr
                (lambda (expr env)
                  (match expr
                    [(? number? n) n]
                    [(? symbol? x) (env x)]
                    [`(quote ,v) v]
                    [`(cons ,a ,d) (cons (eval-expr a env) (eval-expr d env))]
                    [`(match ,e1
                        ['() ,e2]
                        [(cons ,(? symbol? v1) ,(? symbol? v2)) ,e3])
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
                    [`(letrec ([,(? symbol? name)
                                (lambda (,(? symbol? arg1) ,(? symbol? arg2))
                                  ,fbody)])
                        ,lbody)
                      (eval-expr
                        lbody
                        (letrec ((env2 (lambda (y)
                                         ((ext-env
                                            name (lambda (a b)
                                                   (eval-expr
                                                     fbody
                                                     (ext-env
                                                       arg1 a
                                                       (ext-env
                                                         arg2 b
                                                         env2))))
                                            env)
                                          y))))
                          env2))]
                    [`(,rator ,rand1 ,rand2)
                      ((eval-expr rator env) (eval-expr rand1 env) (eval-expr rand2 env))]))))
       (eval-expr ',expr
                  ,initial-interp-env))))

(display
  (time (run* (q)
          (evalo
            (interp-eval
              `(letrec ([append (lambda (l s)
                                  (match l
                                    ['() s]
                                    [(cons a d) (cons a (append d s))]))])
                 (append '(1 2) '(3 4))))
            q))))

(display "\n")

(display
  (time
    (run* (q)
      (evalo
        (interp-eval
          `(letrec ([append (lambda (l s)
                              (match l
                                ['() s]
                                [(cons a d) (cons a (append d s))]))])
             (append ',q '(3 4))))
        '(1 2 3 4)))))

(display "\n")



