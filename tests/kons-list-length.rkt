#lang racket

(require "../main.rkt"
         "kons-list.rkt"
         (prefix-in l: racket/list)
         (prefix-in r: racket/base))

(provide (struct-out kons-list/length))

(struct kons-list/length (l elts) #:transparent
        #:methods gen:collection
        [(define (empty? k)
           (l:empty? (kons-list/length-elts k)))
         (define (first k)
           (car (kons-list/length-elts k)))
         (define (rest k)
           (kons-list/length (sub1 (kons-list/length-l k))
                             (cdr (kons-list/length-elts k))))
         (define (make-empty k)
           (kons-list/length 0 '()))
         (define (cons x k)
           (kons-list/length (add1 (kons-list/length-l k))
                             (r:cons x (kons-list/length-elts k))))
         ;; overrides
         (define (length k)
           (kons-list/length-l k))
         (define (foldl f b k . cs)
           'dummy)])

(module+ test
  (require rackunit)

  (let ()
    (define mt (kons-list/length 0 '()))

    (check-equal? (length (kons-list/length 0 '())) 0)
    (check-equal? (length (kons-list/length 1 '(1))) 1)
    (check-equal? (length (kons-list/length 3 '(1 2 3))) 3)
    (check-equal? (length (kons-list/length -2 '(1 2 3))) -2)
    (check-equal? (foldl 0 + (kons-list/length 1 '(1))) 'dummy)

    (check-equal? (foldr cons mt (kons-list/length 3 '(1 2 3)))
                  (kons-list/length 3 '(1 2 3)))
    (check-equal? (foldr cons mt (kons-list/length -2 '(1 2 3)))
                  (kons-list/length 3 '(1 2 3)))
    (check-equal? (range #:collection mt 4) (kons-list/length 4 '(0 1 2 3)))
    (check-equal? (make #:collection mt 4 'a) (kons-list/length 4 '(a a a a)))
    (check-equal? (build #:collection mt 4 add1) (kons-list/length 4 '(1 2 3 4)))

    (check-equal? (map add1 (kons-list/length 4 '(1 2 3 4)))
                  (kons-list/length 4 '(2 3 4 5)))
    (check-equal? (filter odd? (kons-list/length 4 '(1 2 3 4)))
                  (kons-list/length 2 '(1 3)))

    (check-equal? (map + (kons-list/length 4 '(1 2 3 4))
                       (kons-list/length 4 '(2 3 4 5)))
                  (kons-list/length 4 '(3 5 7 9)))
    ;; heterogeneous case, first collection type wins
    (check-equal? (map + (kons-list/length 4 '(1 2 3 4))
                       (kons-list '(2 3 4 5)))
                  (kons-list/length 4 '(3 5 7 9)))

    (check-equal? (reverse (kons-list/length 3 '(1 2 3)))
                  (kons-list/length 3 '(3 2 1)))
    ))
