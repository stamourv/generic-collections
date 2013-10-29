#lang racket

(require "../main.rkt" "kons-list.rkt"
         racket/generic)

(struct range-struct (min max) #:transparent
        #:methods gen:collection
        ;; make sure that this is overridden, even when calling range
        [(define/generic generic-build build)
         (define (range c n [m #f] [step #f])
           ;; not a complete or correct implementation
           (generic-build (kons-list '())
                          (- (range-struct-max c) (range-struct-min c))
                          (lambda (x) (+ x (range-struct-min c)))))])

(module+ test
  (require rackunit)

  (let ()

    ;; tests for optional dispatch for builders
    
    (check-equal? (range #:collection (range-struct 4 6) #f)
                  (kons-list '(4 5)))
    (check-equal? (range 4) '(0 1 2 3))
    (check-equal? (range 4 6) '(4 5))
    (check-equal? (range 4 -2 -2) '(4 2 0))
    (check-equal? (range #:collection (kons-list '()) 4)
                  (kons-list '(0 1 2 3)))
    (check-equal? (range #:collection (kons-list '()) 4 6)
                  (kons-list '(4 5)))
    (check-equal? (range #:collection (kons-list '()) 4 -2 -2)
                  (kons-list '(4 2 0)))

    (check-equal? (make 5 5) '(5 5 5 5 5))
    (check-equal? (make #:collection (kons-list '()) 5 5)
                  (kons-list '(5 5 5 5 5)))
    (check-equal? (build 5 values) '(0 1 2 3 4))
    (check-equal? (build #:collection (kons-list '()) 5 values)
                  (kons-list '(0 1 2 3 4)))
))
