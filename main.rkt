#lang racket

(require racket/generic
         [prefix-in l: racket/list])

(define (fallback-length c)
  ;; TODO check if the right methods are implemented
  ;; TODO extend with indexed traversals
  (if (empty? c)
      0
      (add1 (length (rest c)))))

(define (fallback-foldr f b c)
  ;; TODO check if the right methods are implemented
  ;; TODO extend with indexed traversals
  (if (empty? c)
      b
      (f (first c) (foldr f b (rest c)))))

(define (fallback-foldl f b c)
  ;; TODO check if the right methods are implemented
  ;; TODO extend with indexed traversals
  (if (empty? c)
      b
      (foldl f (f (first c) b) (rest c))))


(define-generics collection
  ;; This interface has the following groups of methods:
  ;;  - structural traversal (a la gen:stream, for list-likes)
  ;;  - indexed traversal (a la iterator pattern, for vector-likes)
  ;;  - derived traversals (fold and co. with fallbacks using either of the
  ;;    above groups of methods)
  ;;  - structural building (a la empty+cons)
  ;;  - indexed building (with an explicit constructor)
  ;;  - derived building (unfold, range and co. again with fallbacks)
  ;;  - "transducers" (for lack of a better name) (map and co. need both
  ;;    a way to traverse and a way to build. again with fallbacks)
  ;;  TODO also a group for in-place (mutation-based) building? or it that
  ;;   covered by indexed building? (I guess order wouldn't be defined, which
  ;;   is a problem for in-place changes. still, can override methods that
  ;;   traverse in wrong order)


  ;; Structural traversal
  [empty? collection]
  [first collection]
  [rest collection]
  
  ;; Indexed traversal
  ;; TODO
  
  ;; Derived traversals
  [length collection]
  [foldr f base collection]
  [foldl f base collection]
  ;; TODO others

  ;; TODO other groups

  #:fallbacks
  [
   ;; Derived traversals, depend on either kind of basic traversals
   (define length fallback-length)
   (define foldr  fallback-foldr)
   (define foldl  fallback-foldl)
   ]
  )


(struct kons-list (elts)
        #:methods gen:collection
        [(define (empty? k)
           (l:empty? (kons-list-elts k)))
         (define (first k)
           (car (kons-list-elts k)))
         (define (rest k)
           (kons-list (cdr (kons-list-elts k))))])

(module+ test
  (require rackunit)

  (check-equal? (length (kons-list '())) 0)
  (check-equal? (length (kons-list '(1))) 1)
  (check-equal? (length (kons-list '(1 2 3))) 3)
  
  (check-equal? (foldr + 0 (kons-list '())) 0)
  (check-equal? (foldr + 0 (kons-list '(1 2 3))) 6)
  (check-equal? (foldr - 0 (kons-list '())) 0)
  (check-equal? (foldr - 0 (kons-list '(1 2 3))) 2)
  (check-equal? (foldl - 0 (kons-list '())) 0)
  (check-equal? (foldl - 0 (kons-list '(1 2 3))) 2)
  ;; TODO have a test that uses (kons-list '()) as the base case, once we have builders
  (check-equal? (foldr cons '() (kons-list '(1 2 3))) '(1 2 3))
  (check-equal? (foldl cons '() (kons-list '(1 2 3))) '(3 2 1))
  )

(struct kons-list/length (l elts)
        #:methods gen:collection
        [(define (empty? k)
           (l:empty? (kons-list/length-elts k)))
         (define (first k)
           (car (kons-list/length-elts k)))
         (define (rest k)
           (kons-list (cdr (kons-list/length-elts k))))
         ;; overrides
         (define (length k)
           (kons-list/length-l k))
         (define (foldr f b k)
           'dummy)])

(module+ test

  (check-equal? (length (kons-list/length 0 '())) 0)
  (check-equal? (length (kons-list/length 1 '(1))) 1)
  (check-equal? (length (kons-list/length 3 '(1 2 3))) 3)
  (check-equal? (length (kons-list/length -2 '(1 2 3))) -2)
  (check-equal? (foldr 0 + (kons-list/length 1 '(1))) 'dummy)
  )
