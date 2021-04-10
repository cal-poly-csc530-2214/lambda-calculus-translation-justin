#lang typed/racket

(require typed/rackunit)

;LC :=  num
;    |  id
;    |  (/ id => LC)
;    |  (LC LC)
;    |  (+ LC LC)
;    |  (* LC LC)
;    |  (ifleq0 LC LC LC)
;    |  (println LC)

(define (compile [s : Sexp]) : String
  (match s
    [(? number? n) (~a n)]
    [(? symbol? s) (~a s)]
    [(list '/ sym '=> LC) (string-append "(lambda " (compile sym) " : " (compile LC) ")")]
    [(list 'println LC) (string-append "print(" (compile LC) ")\n")]
    [(list LC1 LC2) (string-append (compile LC1) "(" (compile LC2) ")")]
    [(list '+ LC1 LC2) (string-append "(" (compile LC1) " + " (compile LC2) ")")]
    [(list '* LC1 LC2) (string-append "(" (compile LC1) " * " (compile LC2) ")")]
    [(list 'ifleq0 cond then else)
     (string-append (compile then) " if " (compile cond) " <= 0 else " (compile else))]))

(check-equal? (compile '(/ x => (+ x 5))) "(lambda x : (x + 5))")
(check-equal? (compile '(/ x => (ifleq0 2 3 4))) "(lambda x : 3 if 2 <= 0 else 4)")
(check-equal? (compile '((/ x => (+ x 5)) 6)) "(lambda x : (x + 5))(6)")
(check-equal? (compile '(println ((/ x => (* x 5)) 2))) "print((lambda x : (x * 5))(2))\n")

