#lang racket
(provide test-runner)
(require rackunit)

(define (test-runner run)
  (check-equal? (run (call-with-values (lambda () (values 1 2)) +)) 3)
  (check-equal? (run (+ (values 2) 4)) 6)
  (check-equal? (run (call-with-values (lambda () 4) (lambda (x) x))) 4)
  (check-equal? (run (begin (values 1 2 3) 4)) 4)
  (check-equal? (run (call-with-values (lambda () (values 1)) add1)) 2)
  (check-equal? (run (call-with-values (lambda () 1) add1)) 2)
  (check-equal? (run (if (values #t) 1 2)) 1)
  (check-equal? (run (call-with-values (lambda () (values 1 2)) (lambda (x y) (+ x y)))) 3)
  (check-equal? (run (call-with-values (lambda () (values)) (lambda args args))) '())