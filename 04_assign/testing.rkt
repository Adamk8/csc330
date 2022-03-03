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
        (cond [(and (= x2 1) (= x1 1)) (cons 1 (lambda () (f x2 (+ x2 x1))))]
              [(= x2 0) (cons 0 (lambda () (f x2 1)))]
              [(= x2 1) (cons 1 (lambda () (f x2 (+ x2 x1))))]
              [#t (cons x2 (lambda () (f x2 (+ x2 x1))))]
             ))])
      (lambda () (f 0 0))
   ) 
 )

(stream-for-n-steps fibo-stream 15)

(define (filter-stream check stream)
    (letrec ([helper (lambda (new-stream)
                     (let ([stream-pr (new-stream)])
                       (if (check (car stream-pr))
                           (lambda () (cons (car stream-pr) (helper (cdr stream-pr))))
                           (helper (cdr stream-pr)))
                       ))])
    (helper stream)
    ))

(stream-for-n-steps
(filter-stream (lambda (i) (> i 5)) nat-num-stream) 5)

(define is-palyndrome
(lambda (x)
    (let* ([str (number->string x)]
             [str-lst (string->list str)])
      (equal? str-lst (reverse str-lst))

      )))

(define palyndromic-numbers
  (filter-stream (lambda (x)
    (let* ([str (number->string x)]
             [str-lst (string->list str)])
      (equal? str-lst (reverse str-lst))

      )) nat-num-stream)
  )

(is-palyndrome 1)
(is-palyndrome 12)
(is-palyndrome 11)
(is-palyndrome 101)
(stream-for-n-steps palyndromic-numbers 20)

(define-syntax create-stream
  (syntax-rules (using starting at with increment)
    [(create-stream e1 using e2 starting at e3 with increment e4)
     (define e1 
     (letrec ([f (lambda(x) (cons (e2 x) (lambda () (f (+ x e4)))))])
       (lambda () (f e3))
       ))
  ]))

                      
(create-stream squares using (lambda (x) (* x x))
starting at (begin (print "starting") 5)
with increment (begin (print "inc") 2))

(squares)
(stream-for-n-steps squares 5)


(define (vector-assoc v vec)
  (letrec ([helper (lambda (index end)
                     (if (> end index)
                         (let ([element (vector-ref vec index)])
                           (cond [(pair? element) (if (= v (car element))
                                                      element
                                                      (helper (+ index 1) end))]
                                 [#t (helper (+ index 1) end)]
                                 ))
                         #f))])
    (helper 0 (vector-length vec))
  ))

(vector-assoc 4 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1)))


(define (cached-assoc xs n)
  (letrec ([cached-vec (make-vector n #f)]
           [vec-pos 0]
           [return-func (lambda (x)
                (let ([ans (vector-assoc x cached-vec)])
                  (if ans
                        ans
                      (let ([new-ans (assoc x xs)])
                        (if (pair? new-ans)
                            (begin
                              (vector-set! cached-vec vec-pos new-ans)
                              (set! vec-pos (modulo (+ vec-pos 1) n))
                              new-ans)
                            #f)))))])
     return-func
    ))


(cached-assoc  (list (cons 1 2) (cons 3 4)) 3) 


(let
    [(cache (cached-assoc  (list (cons 1 2) (cons 3 4)) 3) )]
  (begin
    (cache 3)
    (cache 3)
    (cache 1)
    (cache 1)
    (cache 2))
)


