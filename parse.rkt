#lang racket
(provide parse parse-define parse-e)
(require "ast.rkt")

;; [Listof S-Expr] -> Prog
(define (parse s)
  (match s
    [(cons (and (cons 'define _) d) s)
     (match (parse s)
       [(Prog ds e)
        (Prog (cons (parse-define d) ds) e)])]
    [(cons e '()) (Prog '() (parse-e e))]
    [_ (error "program parse error")]))

;; S-Expr -> Defn
(define (parse-define s)
  (match s
    [(list 'define (list-rest (? symbol? f) xs) e)
     (if (andmap symbol? xs)
         (Defn f xs (parse-e e))
         (error "parse definition error"))]
    [_ (error "Parse defn error" s)]))

;; S-Expr -> Expr
(define (parse-e s)
  (match s
    [(? integer?)                  (Int s)]
    [(? boolean?)                  (Bool s)]
    [(? char?)                     (Char s)]
    [(? string?)                   (Str s)]
    ['eof                          (Eof)]
    [(? symbol?)                   (Var s)]
    [(list 'quote (list))          (Empty)]
    [(list (? (op? op0) p0))       (Prim0 p0)]
    [(list (? (op? op1) p1) e)     (Prim1 p1 (parse-e e))]
    [(list (? (op? op2) p2) e1 e2) (Prim2 p2 (parse-e e1) (parse-e e2))]
    [(list (? (op? op3) p3) e1 e2 e3)
     (Prim3 p3 (parse-e e1) (parse-e e2) (parse-e e3))]
    [(cons 'values es)
     (Mv-values (map parse-e es))]
    [(list 'call-with-values prod con)
     (parse-call-with-values prod con)]
    [(list 'mv-let e ids body)
     (Mv-let (parse-e e) ids (parse-e body))]
    [(list 'begin e1 e2)
     (Begin (parse-e e1) (parse-e e2))]
    [(list 'if e1 e2 e3)
     (If (parse-e e1) (parse-e e2) (parse-e e3))]
    [(list 'let (list (list (? symbol? x) e1)) e2)
     (Let x (parse-e e1) (parse-e e2))]
    [(cons 'match (cons e ms))
     (parse-match (parse-e e) ms)]
    [(list (or 'lambda 'λ) xs e)
     (if (and (list? xs)
              (andmap symbol? xs))
         (Lam (gensym 'lambda) xs (parse-e e))
         (error "parse lambda error"))]
    [(cons e es)
     (App (parse-e e) (map parse-e es))]    
    [_ (error "Parse error" s)]))

(define (parse-call-with-values producer consumer)
  (let ([new-producer (remove-producer-lam producer)])
    (match new-producer

      ;;mv-values case
      [(cons 'values xs)
       (match (length xs)
      [1
       (match consumer
         [(list (or 'lambda 'λ) arg body) (if (eq? (length arg) (length xs))
                                            (Mv-let (parse-e (car xs)) arg (parse-e body))
                                            (error "producer output != consumer input" xs)) ]


         [_ (parse-e (list consumer (car xs)))])]
         
      [_
       (match consumer
         [(list (or 'lambda 'λ) arg body) (if (eq? (length arg) (length xs))
                                              (Mv-let (Mv-values (map parse-e xs)) arg (parse-e body))
                                              (error "producer output != consumer input" xs)) ]
         [_ (parse-e (cons consumer xs))])])]
      
      ;;application case
      [(cons e es)
       (match consumer
       [(list (or 'lambda 'λ) xs body) (Mv-let (parse-e producer) xs (parse-e body))]
       [_
        (match consumer
          [(? (op? op0) p) (Mv-let (parse-e producer) '() (parse-e (cons consumer '())))]
          [(? (op? op1) p) (Mv-let (parse-e producer) '(x) (parse-e (cons consumer '(x))))]
          [(? (op? op2) p) (Mv-let (parse-e producer) '(x y) (parse-e (cons consumer '(x y))))]
          [(? (op? op3) p) (Mv-let (parse-e producer) '(x y z) (parse-e (cons consumer '(x y z))))]
          [_ (Mv-call (parse-e producer) (parse-e (cons consumer '())))])])]

      
      ;;single value case
      [_
       (match consumer
         [(list (or 'lambda 'λ) arg body) (if (eq? (length arg) 1)
                                            (Mv-let (parse-e new-producer) arg (parse-e body))
                                            (error "producer output != consumer input" (length arg))) ]
       [_ (parse-e (list consumer new-producer))])])))

(define (remove-producer-lam producer)
  (match producer
    [(list (or 'lambda 'λ) xs new-producer)
     (if (and (list? xs) (eq? (length xs) 0))
         new-producer
         (error "call-with-values' producer cannot accept any arguments" xs))]
    [_ producer]))

(define (parse-match e ms)
  (match ms
    ['() (Match e '() '())]
    [(cons (list p r) ms)
     (match (parse-match e ms)
       [(Match e ps es)
        (Match e
               (cons (parse-pat p) ps)
               (cons (parse-e r) es))])]))

(define (parse-pat p)
  (match p
    [(? boolean?) (PLit p)]
    [(? integer?) (PLit p)]
    [(? char?)    (PLit p)]
    ['_           (PWild)]
    [(? symbol?)  (PVar p)]
    [(list 'quote (list))
     (PLit '())]
    [(list 'box p)
     (PBox (parse-pat p))]
    [(list 'cons p1 p2)
     (PCons (parse-pat p1) (parse-pat p2))]
    [(list 'and p1 p2)
     (PAnd (parse-pat p1) (parse-pat p2))]))

(define op0
  '(read-byte peek-byte void))

(define op1
  '(add1 sub1 zero? char? write-byte eof-object?
         integer->char char->integer
         box unbox empty? cons? box? car cdr
         vector? vector-length string? string-length))
(define op2
  '(+ - < = cons eq? make-vector vector-ref make-string string-ref))
(define op3
  '(vector-set!))
(define opN
  '(+ values))

(define (op? ops)
  (λ (x)
    (and (symbol? x)
         (memq x ops))))
