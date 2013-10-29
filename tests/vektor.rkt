#lang racket

(require "../main.rkt" "kons-list.rkt" "kons-list-length.rkt"
         (prefix-in r: racket/base))

(struct vektor (v) #:transparent
        #:methods gen:collection
        [(define (make-iterator v)
           (vektor-iterator (vektor-v v) 0 (vector-length (vektor-v v))))
         (define (make-builder v)
           (vektor-builder '()))])

(struct vektor-iterator (v i l) #:mutable #:transparent
        #:methods gen:iterator
        [(define (has-next? v)
           (< (vektor-iterator-i v) (vektor-iterator-l v)))
         (define (next v)
           (begin0 (vector-ref (vektor-iterator-v v) (vektor-iterator-i v))
             (set-vektor-iterator-i! v (add1 (vektor-iterator-i v)))))])

;; Uses an intermediate list. Not sure how to do better without "size hints".
(struct vektor-builder (l) #:mutable #:transparent
        #:methods gen:builder
        [(define (add-next x v)
           (set-vektor-builder-l! v (r:cons x (vektor-builder-l v))))
         (define (finalize v)
           (vektor (list->vector (r:reverse (vektor-builder-l v)))))])

(module+ test
  (require rackunit)

  (let ()
    (define mt (vektor '#()))

    (check-equal? (length (vektor '#(1 2 3))) 3)

    (check-equal? (foldr + 0 (vektor '#())) 0)
    (check-equal? (foldr + 0 (vektor '#(1 2 3))) 6)
    (check-equal? (foldr - 0 (vektor '#())) 0)
    (check-equal? (foldr - 0 (vektor '#(1 2 3))) 2)
    (check-equal? (foldl - 0 (vektor '#())) 0)
    (check-equal? (foldl - 0 (vektor '#(1 2 3))) 2)
    (check-equal? (foldr r:cons '() (vektor '#(1 2 3))) '(1 2 3))
    (check-equal? (foldl r:cons '() (vektor '#(1 2 3))) '(3 2 1))

    (check-equal? (range #:collection mt 4) (vektor '#(0 1 2 3)))
    (check-equal? (make #:collection mt 4 'a) (vektor '#(a a a a)))
    (check-equal? (build #:collection mt 4 add1) (vektor '#(1 2 3 4)))

    (check-equal? (map add1 (vektor '#(1 2 3 4))) (vektor '#(2 3 4 5)))
    (check-equal? (filter odd? (vektor '#(1 2 3 4))) (vektor '#(1 3)))

    (check-equal? (map + (vektor '#(1 2 3 4))
                       (vektor '#(2 3 4 5)))
                  (vektor '#(3 5 7 9)))
    ;; heterogeneous case, first collection type wins
    (check-equal? (map + (kons-list/length 4 '(1 2 3 4))
                       (vektor '#(2 3 4 5)))
                  (kons-list/length 4 '(3 5 7 9)))

    (check-equal? (foldr list 'x
                         (range #:collection (vektor '#()) 3)
                         (kons-list '(5 6 7))
                         (vektor '#(11 12 13)))
                  '(0 5 11 (1 6 12 (2 7 13 x))))
    (check-equal? (foldl list 'x
                         (range #:collection (vektor '#()) 3)
                         (kons-list '(5 6 7))
                         (vektor '#(11 12 13)))
                  '(2 7 13 (1 6 12 (0 5 11 x))))

    (check-equal? (reverse (vektor '#(1 2 3))) (vektor '#(3 2 1)))

    (check-equal? (andmap positive? (vektor '#(1 2 3 4))) #t)
    (check-equal? (andmap positive? (vektor '#(1 2 3 -4))) #f)
    (check-equal? (andmap < (vektor '#(1 2 3)) (vektor '#(4 5 6))) #t)
    (check-equal? (andmap < (vektor '#(1 2 3)) (vektor '#(4 5 0))) #f)
    (check-equal? (andmap < (vektor '#(1 2 3)) '(4 5 6)) #t)
    (check-equal? (andmap < (vektor '#(1 2 3)) '(4 5 0)) #f)
    (check-equal? (ormap positive? (vektor '#(-1 2 3 4))) #t)
    (check-equal? (ormap positive? (vektor '#(-1 -2 -3 -4))) #f)
    (check-equal? (ormap < (vektor '#(1 2 3)) (vektor '#(0 1 6))) #t)
    (check-equal? (ormap < (vektor '#(1 2 3)) (vektor '#(0 0 0))) #f)
    (check-equal? (ormap < (vektor '#(1 2 3)) '(0 1 6)) #t)
    (check-equal? (ormap < (vektor '#(1 2 3)) '(0 0 0)) #f)

    (check-equal? (ref (vektor '#(1 2 3)) 0) 1)
    (check-equal? (ref (vektor '#(1 2 3)) 2) 3)
    (check-equal? (with-output-to-string
                    (lambda () (for-each display (vektor '#(1 2 3)))))
                  "123")
    (check-equal? (with-output-to-string
                    (lambda () (for-each (lambda (x y) (display (- x y)))
                                         (kons-list '(1 2 3))
                                         (kons-list '(0 1 2)))))
                  "111")

    (check-equal? (member? 3 (vektor '#(1 2 3))) 3)
    (check-exn #rx"element not found"
               (lambda () (member? 4 (vektor '#(1 2 3)))))
    (check-equal? (member? 3 (vektor '#(1 2 3)) #:equal? =) 3)
    (check-equal? (member? 4 (vektor '#(1 2 3)) #:equal? (lambda _ #t)) 1)
    (check-equal? (member? 4 (vektor '#(1 2 3)) 'fail) 'fail)
    (check-equal? (member? 4 (vektor '#(1 2 3)) (lambda () 'fail)) 'fail)

    (check-equal? (append (vektor '#(1 2))
                          (vektor '#())
                          (vektor '#(3 4)))
                  (vektor '#(1 2 3 4)))
    (check-equal? (append (vektor '#(1 2)))
                  (vektor '#(1 2)))
    (check-equal? (append (vektor '#(-1 0)) (kons-list '(1 2)) '(3 4))
                  (vektor '#(-1 0 1 2 3 4)))

    (check-equal? (remove 1 (vektor '#(1 1 2 3))) (vektor '#(1 2 3)))
    (check-equal? (remove 4 (vektor '#(1 1 2 3))) (vektor '#(1 1 2 3)))
    (check-equal? (remove* 1 (vektor '#(1 1 2 3))) (vektor '#(2 3)))
    (check-equal? (remove* 4 (vektor '#(1 1 2 3))) (vektor '#(1 1 2 3)))

    (check-equal? (last (vektor '#(1 2 3))) 3)

    (check-equal? (take (vektor '#(1 2 3)) 0) (vektor '#()))
    (check-equal? (take (vektor '#(1 2 3)) 2) (vektor '#(1 2)))
    (check-equal? (drop (vektor '#(1 2 3)) 0) (vektor '#(1 2 3)))
    (check-equal? (drop (vektor '#(1 2 3)) 2) (vektor '#(3)))
    (check-equal? (call-with-values
                      (lambda () (split-at (vektor '#(1 2 3)) 0))
                    r:cons)
                  (r:cons (vektor '#()) (vektor '#(1 2 3))))
    (check-equal? (call-with-values
                      (lambda () (split-at (vektor '#(1 2 3)) 2))
                    r:cons)
                  (r:cons (vektor '#(1 2)) (vektor '#(3))))
    (check-equal? (take-right (vektor '#(1 2 3)) 0) (vektor '#()))
    (check-equal? (take-right (vektor '#(1 2 3)) 2) (vektor '#(2 3)))
    (check-equal? (drop-right (vektor '#(1 2 3)) 0) (vektor '#(1 2 3)))
    (check-equal? (drop-right (vektor '#(1 2 3)) 2) (vektor '#(1)))
    (check-equal? (call-with-values
                      (lambda () (split-at-right (vektor '#(1 2 3)) 0))
                    r:cons)
                  (r:cons (vektor '#(1 2 3)) (vektor '#())))
    (check-equal? (call-with-values
                      (lambda () (split-at-right (vektor '#(1 2 3)) 2))
                    r:cons)
                  (r:cons (vektor '#(1)) (vektor '#(2 3))))
    ))
