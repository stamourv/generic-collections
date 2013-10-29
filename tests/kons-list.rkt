#lang racket

(require "../main.rkt"
         (prefix-in l: racket/list)
         (prefix-in r: racket/base))

(provide (struct-out kons-list))

;; TODO actually, not really a kons list. try a real one too (spine of structs)
(struct kons-list (elts) #:transparent
        #:methods gen:collection
        [(define (empty? k)
           (l:empty? (kons-list-elts k)))
         (define (first k)
           (car (kons-list-elts k)))
         (define (rest k)
           (kons-list (cdr (kons-list-elts k))))
         (define (make-empty k)
           (kons-list '()))
         (define (cons x k)
           (kons-list (r:cons x (kons-list-elts k))))])

(module+ test
  (require rackunit)

  (let ()
    (define mt (kons-list '()))
    (check-equal? (length (kons-list '())) 0)
    (check-equal? (length (kons-list '(1))) 1)
    (check-equal? (length (kons-list '(1 2 3))) 3)
    
    (check-equal? (foldr + 0 (kons-list '())) 0)
    (check-equal? (foldr + 0 (kons-list '(1 2 3))) 6)
    (check-equal? (foldr - 0 (kons-list '())) 0)
    (check-equal? (foldr - 0 (kons-list '(1 2 3))) 2)
    (check-equal? (foldl - 0 (kons-list '())) 0)
    (check-equal? (foldl - 0 (kons-list '(1 2 3))) 2)
    (check-equal? (foldr r:cons '() (kons-list '(1 2 3))) '(1 2 3))
    (check-equal? (foldl r:cons '() (kons-list '(1 2 3))) '(3 2 1))
    (check-equal? (foldr cons mt (kons-list '(1 2 3))) (kons-list '(1 2 3)))
    (check-equal? (foldl cons mt (kons-list '(1 2 3))) (kons-list '(3 2 1)))

    (check-equal? (range #:collection mt 4) (kons-list '(0 1 2 3)))
    (check-equal? (make #:collection mt 4 'a) (kons-list '(a a a a)))
    (check-equal? (build #:collection mt 4 add1) (kons-list '(1 2 3 4)))

    (check-equal? (map add1 (kons-list '(1 2 3 4))) (kons-list '(2 3 4 5)))
    (check-equal? (filter odd? (kons-list '(1 2 3 4))) (kons-list '(1 3)))

    (check-equal? (map + (kons-list '(1 2 3 4)) (kons-list '(2 3 4 5)))
                  (kons-list '(3 5 7 9)))
    (check-exn exn:fail:contract:arity?
               (lambda ()
                 (filter (lambda (x y) (< 5 (+ x y)))
                         (kons-list '(1 2 3 4)) (kons-list '(2 3 4 5)))))

    (check-equal? (foldr list 'x
                         (range #:collection (kons-list '()) 3)
                         (kons-list '(5 6 7))
                         (kons-list '(11 12 13)))
                  '(0 5 11 (1 6 12 (2 7 13 x))))
    (check-equal? (foldl list 'x
                         (range #:collection (kons-list '()) 3)
                         (kons-list '(5 6 7))
                         (kons-list '(11 12 13)))
                  '(2 7 13 (1 6 12 (0 5 11 x))))
    
    (check-equal? (reverse (kons-list '(1 2 3))) (kons-list '(3 2 1)))

    ;; taken from range's test suite
    (check-equal? (range #:collection mt 4) (kons-list '(0 1 2 3)))
    (check-equal? (range #:collection mt 0) (kons-list '()))
    (check-equal? (range #:collection mt 8) (kons-list '(0 1 2 3 4 5 6 7)))
    (check-equal? (range #:collection mt 3 2) (kons-list '()))
    (check-equal? (range #:collection mt 3 2 -1) (kons-list '(3)))
    (check-equal? (range #:collection mt 3 9) (kons-list '(3 4 5 6 7 8)))
    (check-equal? (range #:collection mt 3 9 2) (kons-list '(3 5 7)))
    (check-equal? (range #:collection mt 3 9 0.5)
                  (kons-list '(3 3.5 4.0 4.5 5.0 5.5 6.0 6.5 7.0 7.5 8.0 8.5)))
    (check-equal? (range #:collection mt 9 3 -2) (kons-list '(9 7 5)))
    (check-equal? (range #:collection mt 10) (kons-list '(0 1 2 3 4 5 6 7 8 9)))
    (check-equal? (range #:collection mt 10 20)
                  (kons-list '(10 11 12 13 14 15 16 17 18 19)))
    (check-equal? (range #:collection mt 20 40 2)
                  (kons-list '(20 22 24 26 28 30 32 34 36 38)))
    (check-equal? (range #:collection mt 20 10 -1)
                  (kons-list '(20 19 18 17 16 15 14 13 12 11)))
    (check-equal? (range #:collection mt 10 15 1.5)
                  (kons-list '(10 11.5 13.0 14.5)))

    (check-equal? (andmap positive? (kons-list '(1 2 3 4))) #t)
    (check-equal? (andmap positive? (kons-list '(1 2 3 -4))) #f)
    (check-equal? (andmap < (kons-list '(1 2 3)) (kons-list '(4 5 6))) #t)
    (check-equal? (andmap < (kons-list '(1 2 3)) (kons-list '(4 5 0))) #f)
    (check-equal? (andmap < (kons-list '(1 2 3)) '(4 5 6)) #t)
    (check-equal? (andmap < (kons-list '(1 2 3)) '(4 5 0)) #f)
    (check-equal? (ormap positive? (kons-list '(-1 2 3 4))) #t)
    (check-equal? (ormap positive? (kons-list '(-1 -2 -3 -4))) #f)
    (check-equal? (ormap < (kons-list '(1 2 3)) (kons-list '(0 1 6))) #t)
    (check-equal? (ormap < (kons-list '(1 2 3)) (kons-list '(0 0 0))) #f)
    (check-equal? (ormap < (kons-list '(1 2 3)) '(0 1 6)) #t)
    (check-equal? (ormap < (kons-list '(1 2 3)) '(0 0 0)) #f)

    (check-equal? (ref (kons-list '(1 2 3)) 0) 1)
    (check-equal? (ref (kons-list '(1 2 3)) 2) 3)
    (check-equal? (with-output-to-string
                    (lambda () (for-each display (kons-list '(1 2 3)))))
                  "123")

    (check-equal? (member? 3 (kons-list '(1 2 3))) 3)
    (check-exn #rx"element not found"
               (lambda () (member? 4 (kons-list '(1 2 3)))))
    (check-equal? (member? 3 (kons-list '(1 2 3)) #:equal? =) 3)
    (check-equal? (member? 4 (kons-list '(1 2 3)) #:equal? (lambda _ #t)) 1)
    (check-equal? (member? 4 (kons-list '(1 2 3)) 'fail) 'fail)
    (check-equal? (member? 4 (kons-list '(1 2 3)) (lambda () 'fail)) 'fail)

    (check-equal? (append (kons-list '(1 2))
                          (kons-list '())
                          (kons-list '(3 4)))
                  (kons-list '(1 2 3 4)))
    (check-equal? (append (kons-list '(1 2)))
                  (kons-list '(1 2)))
    (check-equal? (append (kons-list '(1 2)) '(3 4))
                  (kons-list '(1 2 3 4)))
    (check-equal? (append '(1 2) (kons-list '(3 4)))
                  '(1 2 3 4))

    (check-equal? (remove 1 (kons-list '(1 1 2 3))) (kons-list '(1 2 3)))
    (check-equal? (remove 4 (kons-list '(1 1 2 3))) (kons-list '(1 1 2 3)))
    (check-equal? (remove* 1 (kons-list '(1 1 2 3))) (kons-list '(2 3)))
    (check-equal? (remove* 4 (kons-list '(1 1 2 3))) (kons-list '(1 1 2 3)))

    (check-equal? (second (kons-list '(1 2 3))) 2)
    (check-equal? (third (kons-list '(1 2 3))) 3)
    (check-equal? (last (kons-list '(1 2 3))) 3)

    (check-equal? (take (kons-list '(1 2 3)) 0) (kons-list '()))
    (check-equal? (take (kons-list '(1 2 3)) 2) (kons-list '(1 2)))
    (check-equal? (drop (kons-list '(1 2 3)) 0) (kons-list '(1 2 3)))
    (check-equal? (drop (kons-list '(1 2 3)) 2) (kons-list '(3)))
    (check-equal? (call-with-values
                      (lambda () (split-at (kons-list '(1 2 3)) 0))
                    r:cons)
                  (r:cons (kons-list '()) (kons-list '(1 2 3))))
    (check-equal? (call-with-values
                      (lambda () (split-at (kons-list '(1 2 3)) 2))
                    r:cons)
                  (r:cons (kons-list '(1 2)) (kons-list '(3))))
    (check-equal? (take-right (kons-list '(1 2 3)) 0) (kons-list '()))
    (check-equal? (take-right (kons-list '(1 2 3)) 2) (kons-list '(2 3)))
    (check-equal? (drop-right (kons-list '(1 2 3)) 0) (kons-list '(1 2 3)))
    (check-equal? (drop-right (kons-list '(1 2 3)) 2) (kons-list '(1)))
    (check-equal? (call-with-values
                      (lambda () (split-at-right (kons-list '(1 2 3)) 0))
                    r:cons)
                  (r:cons (kons-list '(1 2 3)) (kons-list '())))
    (check-equal? (call-with-values
                      (lambda () (split-at-right (kons-list '(1 2 3)) 2))
                    r:cons)
                  (r:cons (kons-list '(1)) (kons-list '(2 3))))
    ))

