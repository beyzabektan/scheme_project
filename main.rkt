;beyzanur bektan
;2019400174
;compiling: yes
;complete: yes
#lang racket

(provide (all-defined-out))

; 10 points
(define := (lambda (var value)
             (list var value)))

; 10 points
(define -- (lambda args
             (list 'let args)))
; 10 points
(define @ (lambda (bindings expr)
            (append bindings expr)))
; 20 points
(define split_at_delim (lambda (delim args)
                        (remove '() (foldr (lambda (head tail)

                        
           (cond
             [(equal? delim head)
               (cons '() tail)]
             [else (cons (cons head (car tail)) (cdr tail))]
               )
                                 )
         (list '()) args))

                         )
  
  )

; 30 points
(define parse_expr (lambda (expr)
       (cond
         
         [(not (equal? (car (split_at_delim '+ expr)) expr))
             (cons '+ (map parse_expr(split_at_delim '+ expr)))
          ]

         [(not (equal? (car (split_at_delim '* expr)) expr))
             (cons '* (map parse_expr(split_at_delim '* expr)))
          ]

         [(not (equal? (car (split_at_delim '@ expr)) expr))
              (append (car (map parse_expr(cons (car (split_at_delim '@ expr)) (cdr (split_at_delim '@ expr))))
               )
                    (cdr (map parse_expr(cons (car (split_at_delim '@ expr)) (cdr (split_at_delim '@ expr))))
               )
                    )
               ]

         [(not (equal? (car (split_at_delim '-- expr)) expr))
              (append (list 'let) (list (map parse_expr(split_at_delim '-- expr))))
          ]

         [(not (equal? (car (split_at_delim ':= expr)) expr))
              (eval (cons ':= (cons (car expr) (cddr expr))))
          ]

         [(number? expr) expr]
         [(list? (car expr)) (parse_expr(car expr))]
         [else (car expr)]
 
        )
                     )
  )

; 20 points
(define eval_expr (lambda (expr)
                    (eval (parse_expr expr))))
