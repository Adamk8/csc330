#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body)
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent)

;; Problem A

;; CHANGE (put your solutions here)
(define (mupllist->racketlist lst)
(if (aunit? lst)
     null
     (let [(head (apair-e1 lst))]
       (cond [(int? head) (cons head (mupllist->racketlist (apair-e2 lst)))]
             [(var? head) (cons head (mupllist->racketlist (apair-e2 lst)))]
             [(apair? head) (cons (mupllist->racketlist head) (mupllist->racketlist (apair-e2 lst)))]
             [#t (error "List element not var or int or apair")]))))

(define (racketlist->mupllist lst)
 (if (null? lst)
     (aunit)
     (let [(head (car lst))]
       
       (cond [(int? head) (apair head (racketlist->mupllist (cdr lst)))]
             [(var? head) (apair head (racketlist->mupllist (cdr lst)))]
             [(list? head) (apair (racketlist->mupllist head) (racketlist->mupllist (cdr lst)))]
             [#t (error "List element not string or number or list")]))))

;; Problem B

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp (see below).
(define (eval-under-env e env)
  (cond [(var? e)
         (envlookup env (var-string e))]
        
        [(add? e)
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1)
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        
        [(int? e) e]
        
        [(ifgreater? e)
         (let [(eval1 (eval-under-env (ifgreater-e1 e) env))
               (eval2 (eval-under-env (ifgreater-e2 e) env))]
         (if (and (int? eval1) (int? eval1))
                   (if (> (int-num eval1) (int-num eval2))
                       (eval-under-env (ifgreater-e3 e) env)
                       (eval-under-env (ifgreater-e4 e) env)
                       )
                   (error (format "bad MUPL expression: ~v" e))))]
        
        [(fun? e)
            (closure env e)]
        
        [(call? e)
         (let [(eval1 (eval-under-env (call-funexp e) env))
               (eval2 (eval-under-env (call-actual e) env))]
           (if (closure? eval1)
               (let* [(local-fun (closure-fun eval1))
                        (f_lbda (cons (cons (fun-formal local-fun) eval2) (cons (cons (fun-nameopt local-fun) eval1)  (closure-env eval1))))
                        (f_rec (cons (cons (fun-formal local-fun) eval2)  (closure-env eval1)))]
                        (if (fun-nameopt local-fun)
                              (eval-under-env (fun-body local-fun) f_lbda)
                              (eval-under-env (fun-body local-fun) f_rec)))
               (error (format "bad MUPL expression: ~v" e))))]
        
        [(mlet? e)
         (let [(eval1 (mlet-var e));(eval-under-env (mlet-var e) env)) Come back to this, do we evaluate mlet-var or not? 
               (eval2 (eval-under-env (mlet-e e) env))]
           (eval-under-env (mlet-body e) (cons (cons eval1 eval2) env)))]
        
        [(apair? e)
         (let [(eval1 (eval-under-env (apair-e1 e) env))
               (eval2 (eval-under-env (apair-e2 e) env))]
           (apair eval1 eval2))]
        
        [(fst? e)
         (let [(eval (eval-under-env (fst-e e) env))]
               (if (apair? eval)
                   (apair-e1 eval)
                   (error (format "bad MUPL expression: ~v" e))))]
        
        [(snd? e)
         (let [(eval (eval-under-env (snd-e e) env))]
         (if (apair? eval)
             (apair-e2 eval)
             (error (format "bad MUPL expression: ~v" e))))]
        
        [(aunit? e) e]
        
        [(isaunit? e)
          (let [(eval (eval-under-env (isaunit-e e) env))]
            (if (aunit? eval)
                (int 1)
                (int 0)))]

        [(closure? e) e]
        
        ;; "CHANGE" add more cases here
        ;; one for each type of expression
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
;; note how evaluating an expression start with an empty environment
(define (eval-exp e)
  (eval-under-env e null))

;; Problem C

(define (ifaunit e1 e2 e3) 
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
    (if (null? lstlst)
        e2
        (let [(pr (car lstlst))]
          (mlet (car pr) (cdr pr) (mlet* (cdr lstlst) e2))
        )
    )
)

(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1
        (mlet "_y" e2
              (let* [(AgtB (ifgreater (var "_x") (var "_y") (int 0) (int 1)))
                     (BgtA (ifgreater (var "_y") (var "_x") (int 1) (int 0)))]
                (ifgreater AgtB BgtA e3 e4))
              )))

;; Problem D

(define mupl-map
  (fun "mupl-map" "f"
   (fun #f "lst"
        (let [(mplist (var "lst"))
              (func (var "f"))]
          (ifaunit mplist
              (aunit)
              (apair (call func (fst mplist)) (call (call (var "mupl-map") func) (snd mplist))))
              ))))

(define mupl-mapAddN
  (mlet "map" mupl-map
        (fun "add-i" "i"
             (call (var "map")
                   (fun #f "x"
                        (add (var "x") (var "i")))))
       ))

;; Internal testing
;;; (define add1 (eval-exp (ifgreater (add (int 2) (int 2)) (add (int 2) (int 1)) (add (int 3) (int -3)) (add "wrong" "bad"))))
;;; (define mlist (racketlist->mupllist (list (int 3) (int 4) (list (int 7) (var "a")) (int 4))))
;;; (define rlist (mupllist->racketlist mlist))
;;; (define inttest (eval-exp (int 5)))
;;; (define pair1 (eval-exp (apair (int 5) (int 2))))
;;; (define gttest (eval-exp (ifgreater (int 10) (int 11) (int 1) (int 0))))
;;; (define fsttest (eval-exp (fst pair1)))
;;; (define sndtest (eval-exp (snd pair1)))
;;; (define unit1 (eval-exp (isaunit (int 10))))
;;; (define unit2 (eval-exp (isaunit (aunit))))
;;; (define let1 (eval-exp (mlet "x" (int 5) (mlet "x" (int 6) (var "x")))))
;;; (define acall (eval-exp (call (fun #f "x" (int 5)) (aunit))))
;;; (define ncall (eval-exp (call (fun #f "x" (add (var "x") (int 4))) (int 99))))
;;; (define nfunc (eval-exp (fun "incr" "x" (int 5))))

;;; (define ifunit (eval-exp (ifaunit (ifgreater (int 10) (int 11) (int 1) (aunit)) (int 2) (int 3))))

;;; (define letstar (eval-exp (mlet* (cons (cons "x" (int 1)) null) (var "x"))))
;;; (define eqtest (eval-exp (ifeq (int 1) (int 1) (int 2) (int 3))))

;;; (define addtwo (fun "addone" "x" (add (var "x") (int 2))))
;;; (define mupl-map-addtwo (call mupl-map addtwo))
;;; (define maptest (eval-exp (call mupl-map-addtwo (aunit))))

;;; (define my-mupl-list (apair (int 23) (apair (int 42) (aunit))))
;;; (define my-answers (apair (int 25) (apair (int 44) (aunit))))
;;; (define maptest2 (eval-exp (call mupl-map-addtwo my-mupl-list)))

;;; (define input (apair (int 25) (apair (int 44) (aunit))))
;;; (define test1 (eval-exp (call mupl-mapAddN (int 1))))
;;; (define addi (eval-exp (call (call mupl-mapAddN (int 1)) input)))


