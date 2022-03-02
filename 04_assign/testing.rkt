#lang racket
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

(define (add-pointwise-lists lst)
  (cond [(null? lst) null]
        [(list? lst) (add-pointwise (car lst) (add-pointwise-lists (cdr lst)))]
        [#t (error "illegal parmeter")])
   )

(define (add-pointwise-lists-2 lst)
  (letrec ([helper (lambda (lst acc) (add-pointwise lst acc))])
    (if (list? lst)
        (foldl helper '() lst) 
        (error "illegal parmeter"))
  ))


(add-pointwise '(1 2 3 5 6) '(2 5))
(add-pointwise '(1) '(2 5))
(add-pointwise '(1 2 3 5 6) '())
(add-pointwise-lists '((1 1) (2 2 2 2) (3) ()))
(add-pointwise-lists '(()))
(add-pointwise-lists-2 '((1 1) (2 2 2 2) (3) ()))
(add-pointwise-lists-2 '(()))

(define (stream-for-n-steps stream n)
  (letrec ([helper (lambda (stream i)
                     (let ([stream-pr (stream)])
                       (if (> i n)
                           '()
                           (cons (car stream-pr) (helper (cdr stream-pr) (+ i 1))))))])
    (helper stream 1)
    ))
                       
(define nat-num-stream
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 0))
    ))

(stream-for-n-steps nat-num-stream 5)
  
(define fibo-stream
    (letrec ([f (lambda (x1 x2)
        (cond [(= x1 0) (cons 0 (lambda () (f x2 (+ x2 x1))))]
             [(= x2 1) (cons 1 (lambda () (f x2 (+ x2 x1))))]
             [#t (cons (+ x1 x2) (lambda () (f x2 (+ x2 x1))))]
             ))])
      (lambda () (f 0 1))
   ) 
 )

(stream-for-n-steps fibo-stream 15)

(define (filter-stream check stream)
    (letrec ([helper (lambda (new-stream)
                     (let ([stream-pr (new-stream)])
                       (if (check (car stream-pr))
                           new-stream
                           (helper (cdr stream-pr)))
                       ))])
    (helper stream)
    ))

(stream-for-n-steps
(filter-stream (lambda (i) (> i 5)) nat-num-stream) 5)

(define is-palyndrome
  (lambda (x)
    (letrec ([str (number->string x)]
             [len (string-length str)])
      (cond [(= len 1) #t]
            [(= (modulo len 2) 0)
             (letrec ([half (quotient len 2)])
               (if (equal? (substring str 0 half) (substring str half))
                   #t
                   #f))]
            [#t (letrec ([half (quotient len 2)])
                  (if (equal? (substring str 0 half) (substring str (+ half 1)))
                      #t
                      #f))]))))

(define (palyndromic-numbers)
  (letrec ([f (lambda (x) (filter-stream is-palyndrome nat-num-stream))])
  (lambda () (f 0))
    )
  )


(stream-for-n-steps palyndromic-numbers 20)

                      
