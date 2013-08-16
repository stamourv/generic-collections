#lang racket

(require racket/generic
         (prefix-in l: racket/list)
         (prefix-in r: racket/base))

(define (can-do-structural-traversal? c)
  (and (collection-implements? c 'empty?)
       (collection-implements? c 'first)
       (collection-implements? c 'rest)))

(define (fallback-length c)
  (unless (can-do-structural-traversal? c)
    (error "cannot traverse collection" c))
  ;; TODO extend with indexed traversals
  (if (empty? c)
      0
      (add1 (length (rest c)))))

(define (fallback-foldr f b c)
  (unless (can-do-structural-traversal? c)
    (error "cannot traverse collection" c))
  ;; TODO extend with indexed traversals
  (if (empty? c)
      b
      (f (first c) (foldr f b (rest c)))))

(define (fallback-foldl f b c)
  (unless (can-do-structural-traversal? c)
    (error "cannot traverse collection" c))
  ;; TODO extend with indexed traversals
  (if (empty? c)
      b
      (foldl f (f (first c) b) (rest c))))

(define (can-do-structural-building? c)
  (and (collection-implements? c 'make-empty)
       (collection-implements? c 'cons)))

(define (fallback-range c n)
  (unless (can-do-structural-building? c)
    (error "cannot build collection" c))
  ;; TODO extend with indexed building
  (let loop ([n (sub1 n)] [acc (make-empty c)])
    (if (< n 0)
        acc
        (loop (sub1 n) (cons n acc)))))

(define (fallback-make c n v)
  (unless (can-do-structural-building? c)
    (error "cannot build collection" c))
  ;; TODO extend with indexed building
  (let loop ([n n] [acc (make-empty c)])
    (if (= n 0)
        acc
        (loop (sub1 n) (cons v acc)))))

(define (fallback-build c n f)
  (unless (can-do-structural-building? c)
    (error "cannot build collection" c))
  ;; TODO extend with indexed building
  (let loop ([n (sub1 n)] [acc (make-empty c)])
    (if (< n 0)
        acc
        (loop (sub1 n) (cons (f n) acc)))))

(define (fallback-map f c)
  (unless (can-do-structural-traversal? c)
    (error "cannot traverse collection" c))
  (unless (can-do-structural-building? c)
    (error "cannot build collection" c))
  ;; TODO indexed too
  ;; TODO dumb, but that's a start
  (foldr (lambda (new acc) (cons (f new) acc)) (make-empty c) c))

(define (fallback-filter f c)
  (unless (can-do-structural-traversal? c)
    (error "cannot traverse collection" c))
  (unless (can-do-structural-building? c)
    (error "cannot build collection" c))
  ;; TODO indexed too
  ;; TODO dumb, but that's a start
  (foldr (lambda (new acc)
           (if (f new)
               (cons new acc)
               acc))
         (make-empty c)
         c))


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

  ;; Structural building
  [make-empty collection] ; returns a new empty coll. (think `(λ (l) '())')
  [cons x collection]
  ;; TODO alternative API: `make-empty' and `get-cons' (returns cons)

  ;; Indexed building
  ;; TODO

  ;; Derived building
  ;; TODO ugh, kind of ugly to take a dummy coll. as arg...
  ;;  need something to dispatch on. maybe also offer monomorphic versions
  ;;  OR have collection be an opt/kw arg and default to lists
  ;;  OR have the option to pass in empty (or maybe make-empty) and cons
  [range collection n] ; TODO extend to fall API, with opt args
  [make  collection n v] ; think make-list
  [build collection n f] ; think build-list
  ;; TODO unfold
  ;; TODO test overrides for these
  ;; TODO others

  ;; Transducers
  [map f collection]
  [filter f collection]
  ;; TODO others

  #:defined-predicate collection-implements?
  #:fallbacks
  [
   ;; Derived traversals, depend on either kind of basic traversals
   (define length fallback-length)
   (define foldr  fallback-foldr)
   (define foldl  fallback-foldl)
   (define range  fallback-range)
   (define make   fallback-make)
   (define build  fallback-build)
   (define map    fallback-map)
   (define filter fallback-filter)
   ]
  ;; TODO add defaults (lists, vectors, etc.)
  )


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

    (check-equal? (range mt 4) (kons-list '(0 1 2 3)))
    (check-equal? (make mt 4 'a) (kons-list '(a a a a)))
    (check-equal? (build mt 4 add1) (kons-list '(1 2 3 4)))

    (check-equal? (map add1 (kons-list '(1 2 3 4))) (kons-list '(2 3 4 5)))
    (check-equal? (filter odd? (kons-list '(1 2 3 4))) (kons-list '(1 3)))
    ))

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
         (define (foldl f b k)
           'dummy)])

(module+ test
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
    (check-equal? (range mt 4) (kons-list/length 4 '(0 1 2 3)))
    (check-equal? (make mt 4 'a) (kons-list/length 4 '(a a a a)))
    (check-equal? (build mt 4 add1) (kons-list/length 4 '(1 2 3 4)))

    (check-equal? (map add1 (kons-list/length 4 '(1 2 3 4)))
                  (kons-list/length 4 '(2 3 4 5)))
    (check-equal? (filter odd? (kons-list/length 4 '(1 2 3 4)))
                  (kons-list/length 2 '(1 3)))
    ))

(struct not-really-a-collection ()
        #:methods gen:collection
        [])

(module+ test
  (check-exn exn:fail? (lambda () (length 'not-a-collection)))
  (check-exn exn:fail? (lambda () (length (not-really-a-collection)))))
