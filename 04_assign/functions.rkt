#lang racket


(provide (all-defined-out)) ;; so we can put tests in a second file

; part 1
(define nat-num-stream
  (letrec
      ([f (lambda (x)
            (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))

;#1
(define (add-pointwise lst_a lst_b)
  (cond [(not (list? lst_a)) (error "illegal parameter")]
        [(not (list? lst_b)) (error "illegal parameter")]
        [#t (cond [(and (null? lst_a) (null? lst_b) '())]
                  [(null? lst_a) (if (number? (car lst_b))
                                     (cons (car lst_b) (add-pointwise '() (cdr lst_b)))
                                     (error "illegal parameter"))]
                  [(null? lst_b) (if (number? (car lst_a))
                                     (cons (car lst_a) (add-pointwise (cdr lst_a) '()))
                                     (error "illegal parameter"))]
                  [#t (if (and (number? (car lst_a)) (number? (car lst_b)))
                              (cons (+ (car lst_a) (car lst_b)) (add-pointwise (cdr lst_a) (cdr lst_b)))
                              (error "illegal parameter"))]
              )
            ]
        )
  )

;#2
(define (add-pointwise-lists lst)
  (cond [(null? lst) null]
        [(list? lst) (add-pointwise (car lst) (add-pointwise-lists (cdr lst)))]
        [#t (error "illegal parmeter")])
   )

;#3
(define (add-pointwise-lists-2 lst)
  (letrec ([helper (lambda (lst acc) (add-pointwise lst acc))])
    (if (list? lst)
        (foldl helper '() lst) 
        (error "illegal parmeter"))
  ))

;#4
(define (stream-for-n-steps stream count)
  (define (stream-up curr)
    (if (< curr count)
        (cons (stream curr) (stream-up (+ curr 1)))
        '()
        )
   )
  (stream-up 0) 
  )

;#5
(define fibo-stream
    (letrec ([f (lambda (x1 x2)
        (cond [(= x1 0) (cons 0 (lambda () (f x2 (+ x2 x1))))]
             [(= x2 1) (cons 1 (lambda () (f x2 (+ x2 x1))))]
             [#t (cons (+ x1 x2) (lambda () (f x2 (+ x2 x1))))]
             ))])
      (lambda () (f 0 1))
   ) 
 )


;#6
(define (filter-stream check stream)
    (letrec ([helper (lambda (new-stream)
                     (let ([stream-pr (new-stream)])
                       (if (check (car stream-pr))
                           new-stream
                           (helper (cdr stream-pr)))
                       ))])
    (helper stream)
    ))

;#7
(define palyndromic-numbers #f)

;#8 macro create-stream

; part 2

;#1
(define vector-assoc #f)

;#2
(define cached-assoc #f)
