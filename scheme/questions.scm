(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.
(define (map proc items)
  'replace-this-line
  (cond ((null? items) nil)
        (else (cons (proc (car items)) (map proc (cdr items))))
  ))

(define (cons-all first rests)
  'replace-this-line
  (cond ((null? rests) rests)
        (else (cons (append (list first) (car rests)) (cons-all first (cdr rests))))
  ))

(define (zip pairs)
  (cons (map car pairs) (cons (map cadr pairs) nil))
  )



;; Problem 17
;; Returns a list of two-element lists (busy, havent met up in awhile, sorry haven't been helpful, you're going through problems so quickly, plan)
(define (enumerate s)
  ; BEGIN PROBLEM 17

  (define (helper index s)
    (cond ((null? s) s)
          (else (cons (cons index (cons (car s) nil)) (helper (+ 1 index) (cdr s))))

    ))
  (helper 0 s)

  )
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  'replace-this-line
  (cond ((= total 0) (list nil))
        ((< total 0) nil)
        ((null? denoms) denoms)
        ((>= total (car denoms))
                  (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms))
                    (list-change total (cdr denoms))))
        (else (list-change total (cdr denoms))))
          )

  ; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (append (list form params) (let-to-lambda body))

           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
          (define zip_list (zip values))
          (define parameters (car zip_list))
          (define args (map let-to-lambda (cadr zip_list)))
          (define parameters_and_body (append (list 'lambda parameters) (let-to-lambda body)))
          (cons parameters_and_body args)

           ; END PROBLEM 19
           ))
        (else
         ; BEGIN PROBLEM 19
          (map let-to-lambda expr)
         ; END PROBLEM 19
         )))
