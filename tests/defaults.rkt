#lang racket

(require "../main.rkt" "kons-list.rkt")

(module+ test
  (require rackunit)

  (let ()
    ;; tests for #:defaults

    (check-equal? (foldr + 0 '(1 2 3 4)) 10)
    (check-equal? (foldl + 0 '(1 2 3 4)) 10)
    (check-equal? (map + '(1 2 3) '(4 5 6)) '(5 7 9))
    (check-equal? (foldr + 0 '(1 2 3 4) '(5 6 7 8)) 36)
    (check-equal? (foldl + 0 '(1 2 3 4) '(5 6 7 8)) 36)
    (check-equal? (map + '(1 2 3) (kons-list '(4 5 6))) '(5 7 9))
    (check-equal? (foldr + 0 '(1 2 3 4) (kons-list '(5 6 7 8))) 36)
    (check-equal? (foldl + 0 '(1 2 3 4) (kons-list '(5 6 7 8))) 36)
    (check-equal? (map + (kons-list '(4 5 6)) '(1 2 3)) (kons-list '(5 7 9)))
    (check-equal? (foldr + 0 (kons-list '(5 6 7 8)) '(1 2 3 4)) 36)
    (check-equal? (foldl + 0 (kons-list '(5 6 7 8)) '(1 2 3 4)) 36)

    (check-equal? (foldr + 0 '#(1 2 3 4)) 10)
    (check-equal? (foldl + 0 '#(1 2 3 4)) 10)
    (check-equal? (map + '#(1 2 3) '#(4 5 6)) '#(5 7 9))
    (check-equal? (foldr + 0 '#(1 2 3 4) '#(5 6 7 8)) 36)
    (check-equal? (foldl + 0 '#(1 2 3 4) '#(5 6 7 8)) 36)
    (check-equal? (map + '#(1 2 3) (kons-list '(4 5 6))) '#(5 7 9))
    (check-equal? (foldr + 0 '#(1 2 3 4) (kons-list '(5 6 7 8))) 36)
    (check-equal? (foldl + 0 '#(1 2 3 4) (kons-list '(5 6 7 8))) 36)
    (check-equal? (map + (kons-list '(4 5 6)) '#(1 2 3)) (kons-list '(5 7 9)))
    (check-equal? (foldr + 0 (kons-list '(5 6 7 8)) '#(1 2 3 4)) 36)
    (check-equal? (foldl + 0 (kons-list '(5 6 7 8)) '#(1 2 3 4)) 36)
    (check-equal? (range #:collection '#() 1 10 2) '#(1 3 5 7 9))

    (check-equal? (member? 3 '(1 2 3)) 3)
    (check-exn #rx"element not found"
               (lambda () (member? 4 '(1 2 3))))
    (check-equal? (member? 3 '(1 2 3) #:equal? =) 3)
    (check-equal? (member? 4 '(1 2 3) #:equal? (lambda _ #t)) 1)
    (check-equal? (member? 4 '(1 2 3) 'fail) 'fail)
    (check-equal? (member? 4 '(1 2 3) (lambda () 'fail)) 'fail)

    (check-equal? (member? 3 '#(1 2 3)) 3)
    (check-exn #rx"element not found"
               (lambda () (member? 4 '#(1 2 3))))
    (check-equal? (member? 3 '#(1 2 3) #:equal? =) 3)
    (check-equal? (member? 4 '#(1 2 3) #:equal? (lambda _ #t)) 1)
    (check-equal? (member? 4 '#(1 2 3) 'fail) 'fail)
    (check-equal? (member? 4 '#(1 2 3) (lambda () 'fail)) 'fail)

    (check-equal? (remove 1 '(1 1 2 3)) '(1 2 3))
    (check-equal? (remove 4 '(1 1 2 3)) '(1 1 2 3))
    (check-equal? (remove 1 '#(1 1 2 3)) '#(1 2 3))
    (check-equal? (remove 4 '#(1 1 2 3)) '#(1 1 2 3))

    (check-equal? (second '(1 2 3)) 2)
    (check-equal? (third '(1 2 3)) 3)
    (check-equal? (second '#(1 2 3)) 2)
    (check-equal? (third '#(1 2 3)) 3)

    ))
