#lang racket

;;; Interface definition (and "documentation") below.

(require racket/stxparam)

;;;---------------------------------------------------------------------------
;;; Fallback implementations

(require racket/generic
         (prefix-in l: racket/list)
         (prefix-in r: racket/base))

(define (can-do-structural-traversal? c)
  (and (collection-implements? c 'empty?)
       (collection-implements? c 'first)
       (collection-implements? c 'rest)))

(define (can-do-stateful-traversal? c)
  (collection-implements? c 'make-iterator))


(define-syntax-parameter -loop   (syntax-rules ()))
(define-syntax-parameter -empty? (syntax-rules ()))
;; returns a list in multi-coll. case
(define-syntax-parameter -first  (syntax-rules ()))
;; To be passed back to the loop.
;; Note: only works right if `-first' has been called.
(define-syntax-parameter -rest   (syntax-rules ()))
;; Get the rest, works only if `-first' has *not* been called.
(define-syntax-parameter -rest!  (syntax-rules ()))


(define-syntax (traversal stx)
  (syntax-case stx ()
    [(_ (non-coll-args ...) (extra-acc ...)
        body-1-coll
        body-n-colls)
     (let ()
       (define single-collection-part ; just missing the `lambda'
         (syntax/loc stx
           ((non-coll-args ... c)
            (cond
             [(can-do-structural-traversal? c)
              (let loop ([acc c] extra-acc ...)
                (syntax-parameterize
                 ([-loop
                   (make-rename-transformer #'loop)]
                  [-empty?
                   (syntax-rules () [(_) (empty? acc)])]
                  [-first
                   (syntax-rules () [(_) (first acc)])]
                  [-rest
                   (syntax-rules () [(_) (rest acc)])]
                  [-rest!
                   (syntax-rules () [(_) (rest acc)])])
                 body-1-coll))]
             [(can-do-stateful-traversal? c)
              (let ([acc (make-iterator c)])
                (let loop (extra-acc ...)
                  (syntax-parameterize
                   ([-loop
                     (...
                      (syntax-rules ()
                        [(_ in extra-arg ...)
                         ;; No need to pass acc around, but can't drop it
                         ;; either (can be side-effectful, like `-rest!').
                         ;; If it's just `-rest', should be compiled away.
                         (begin in (loop extra-arg ...))]))]
                    [-empty?
                     (syntax-rules () [(_) (not (has-next? acc))])]
                    [-first
                     (syntax-rules () [(_) (next acc)])]
                    [-rest
                     (syntax-rules () [(_) acc])]
                    [-rest!
                     (syntax-rules () [(_) (begin (next acc) acc)])])
                   body-1-coll)))]
             [else
              (error "cannot traverse collection" c)]))))

       (define multi-collection-part
         (syntax/loc stx
           ((non-coll-args ... c . cs)
            (define colls (r:cons c cs))
            (unless (for/and ([coll (in-list colls)])
                      (or (can-do-structural-traversal? coll)
                          can-do-stateful-traversal? coll))
              ;; TODO have better error message than that
              (error "cannot traverse one of" colls))
            ;; While this may look like a (premature) optimization, it's not.
            ;; The goal is not so much to avoid checking what kind of collection
            ;; each thing is every iteration, but rather to make sure we treat
            ;; collections consistently throughout the traversal. If, e.g., the
            ;; iterator for one of the statefully-traversible collections turns
            ;; out to be structurally-traversible, we still want to iterate over
            ;; it statefully, as we first decided. Same for the mirror case of a
            ;; structurally-traversible collection that also happens to be an
            ;; iterator. This may turn out to be irrelevant.
            (define structural? (r:map can-do-structural-traversal? colls))
            (define iterator-likes (for/list ([coll (in-list colls)]
                                              [s?   (in-list structural?)])
                                     (if s? coll (make-iterator coll))))
            (define (mt? it s?) (if s? (empty? it) (not (has-next? it))))
            ;; TODO look up methods up front, if possible
            (let loop ([its iterator-likes] extra-acc ...)
              (syntax-parameterize
               ([-loop
                 (make-rename-transformer #'loop)]
                [-empty?
                 (syntax-rules ()
                   [(_)
                    (and (r:ormap mt? its structural?) ; any empty?
                         (or (r:andmap mt? its structural?) ; all empty?
                             (error "all collections must have same size")))])]
                [-first
                 (syntax-rules ()
                   [(_)
                    (for/list ([it (in-list its)]
                               [s? (in-list structural?)])
                      (if s? (first it) (next it)))])]
                [-rest
                 (syntax-rules ()
                   [(_)
                    (for/list ([it (in-list its)]
                               [s? (in-list structural?)])
                      (if s? (rest it) it))])]) ; already stepped
               body-n-colls)))))

       (cond
        [(and (syntax->datum #'body-1-coll)
              (syntax->datum #'body-n-colls))
         (quasisyntax/loc stx
           (case-lambda
             [#,@single-collection-part]
             [#,@multi-collection-part]))]
        [(syntax->datum #'body-1-coll)
         (quasisyntax/loc stx (lambda #,@single-collection-part))]
        [(syntax->datum #'body-n-colls)
         (quasisyntax/loc stx (lambda #,@multi-collection-part))]
        [else
         (raise-syntax-error
          'traversal "need to provide at least one body" stx)]))]))
;; TODO implement transducer using traversal?

(define fallback-foldr
  (traversal
   (f base) ()
   (if (-empty?)
       base
       (f (-first) (-loop (-rest))))
   (if (-empty?)
       base
       ;; TODO can I do it without the append?
       (apply f (r:append (-first) (list (-loop (-rest))))))))

(define fallback-length
  (traversal
   () ([n 0])
   (if (-empty?)
       n
       (-loop (-rest!) (add1 n)))
   #f))

(define fallback-foldl
  (traversal
   (f base) ([acc base])
   (if (-empty?)
       acc
       (-loop (-rest) (f (-first) acc)))
   (if (-empty?)
       acc
       ;; TODO can I do it without the append?
       (-loop (-rest) (apply f (r:append (-first) (list acc)))))))

(define (can-do-structural-building? c)
  (and (collection-implements? c 'make-empty)
       (collection-implements? c 'cons)))

(define (can-do-stateful-building? c)
  (collection-implements? c 'make-builder))

(define (fallback-range c n)
  (cond
   [(can-do-structural-building? c)
    (let loop ([n (sub1 n)] [acc (make-empty c)])
      (if (< n 0)
          acc
          (loop (sub1 n) (cons n acc))))]
   [(can-do-stateful-building? c)
    (define builder (make-builder c))
    (let loop ([i 0])
      (when (< i n)
        (add-next i builder)
        (loop (add1 i))))
    (finalize builder)]
   [else
    (error "cannot build collection" c)]))

(define (fallback-make c n v)
  (cond
   [(can-do-structural-building? c)
    (let loop ([n n] [acc (make-empty c)])
      (if (= n 0)
          acc
          (loop (sub1 n) (cons v acc))))]
   [(can-do-stateful-building? c)
    (define builder (make-builder c))
    (let loop ([i 0])
      (when (< i n)
        (add-next v builder)
        (loop (add1 i))))
    (finalize builder)]
   [else
    (error "cannot build collection" c)]))

(define (fallback-build c n f)
  (cond
   [(can-do-structural-building? c)
    (let loop ([n (sub1 n)] [acc (make-empty c)])
      (if (< n 0)
          acc
          (loop (sub1 n) (cons (f n) acc))))]
   [(can-do-stateful-building? c)
    (define builder (make-builder c))
    (let loop ([i 0])
      (when (< i n)
        (add-next (f i) builder)
        (loop (add1 i))))
    (finalize builder)]
   [else
    (error "cannot build collection" c)]))


;; This abstracts over the pattern of traversing over n collections, each
;; either structural or stateful, and building a new collection of the same
;; type as the first collection argument, either structurally or statefully.
;; TODO is the 1-coll special case worth it? pretty ugly
(define-syntax-rule (transducer (non-coll-args ...)
                                (elt elts acc stateful-builder)
                                (structural-body-1-coll)
                                (stateful-body-1-coll)
                                (structural-body-n-coll)
                                (stateful-body-n-coll))
  (case-lambda
    ;; TODO worth special-casing the one collection case? that would be for
    ;;  performance only, I guess, and performance-sensitive cases will be
    ;;  covered by the defaults anyway
    [(non-coll-args ... c)
     (cond
      [(and (can-do-structural-traversal? c)
            (can-do-structural-building? c))
       ;; TODO dumb, but that's a start
       (foldr (lambda (elt acc) structural-body-1-coll) (make-empty c) c)]
      [(and (can-do-stateful-traversal? c)
            (can-do-stateful-building? c))
       (define iterator         (make-iterator c))
       (define stateful-builder (make-builder  c))
       (let loop ()
         (when (has-next? iterator)
           (define elt (next iterator))
           stateful-body-1-coll
           (loop)))
       (finalize stateful-builder)]
      ;; TODO are the structural / stateful combinations interesting?
      [else
       (unless (or (can-do-structural-traversal? c)
                   (can-do-stateful-traversal? c))
         (error "cannot traverse collection" c))
       (unless (or (can-do-structural-building? c)
                   (can-do-stateful-building? c))
         (error "cannot build collection" c))])]
    [(non-coll-args ... c . cs)
     (define colls (r:cons c cs))
     (unless (for/and ([coll (in-list colls)])
               ;; TODO are the structural / stateful combinations interesting?
               (or (and (can-do-structural-traversal? coll)
                        (can-do-structural-building? coll))
                   (and (can-do-stateful-traversal? coll)
                        (can-do-stateful-building? coll))))
       ;; TODO have better error message than that
       (error "cannot traverse or build one of" colls))
     ;; While this may look like a (premature) optimization, it's not.
     ;; The goal is not so much to avoid checking what kind of collection
     ;; each thing is every iteration, but rather to make sure we treat
     ;; collections consistently throughout the traversal. If, e.g., the
     ;; iterator for one of the statefully-traversible collections turns
     ;; out to be structurally-traversible, we still want to iterate over
     ;; it statefully, as we first decided. Same for the mirror case of a
     ;; structurally-traversible collection that also happens to be an
     ;; iterator. This may turn out to be irrelevant.
     (define structural? (r:map can-do-structural-building? colls))
     (define structural-building? (l:first structural?))
     (define stateful-builder (if structural-building? #f (make-builder c)))
     (define iterator-likes (for/list ([coll (in-list colls)]
                                       [s?   (in-list structural?)])
                              (if s? coll (make-iterator coll))))
     ;; If we're building structurally, build on the way back from the end.
     ;; If we're building statefully, build on the way to the end.
     ;; TODO look up methods up front, if possible
     (let loop ([its iterator-likes])
       (define (mt? it s?) (if s? (empty? it) (not (has-next? it))))
       (cond
        [(r:ormap mt? its structural?) ; any empty?
         (unless (r:andmap mt? its structural?) ; all empty?
           (error "all collections must have same size"))
         (if structural-building?
             (make-empty c)
             (finalize stateful-builder))]
        [else ; keep going
         (define elts
           (for/list ([it (in-list its)]
                      [s? (in-list structural?)])
             (if s? (first it) (next it))))
         (define nexts
           (for/list ([it (in-list its)]
                      [s? (in-list structural?)])
             (if s? (rest it) it))) ; already stepped
         (cond
          [structural-building?
           (define acc (loop nexts))
           structural-body-n-coll]
          [else ; stateful building
           stateful-body-n-coll
           (loop nexts)])]))])) ; returns finalized builder

(define fallback-map
  (transducer (f) (elt elts acc builder)
              ((cons (f elt) acc))
              ((add-next (f elt) builder))
              ((cons (apply f elts) acc))
              ((add-next (apply f elts) builder))))
(define fallback-filter
  (transducer (f) (elt elts acc builder)
              ((if (f elt)
                   (cons elt acc)
                   acc))
              ((when (f elt)
                 (add-next elt builder)))
              ;; TODO the n-ary template is still generated, but can't run
              ;;  (the generic function raises an arity error before we even
              ;;  get here)
              (#f)
              (#f)))


;;;---------------------------------------------------------------------------
;;; Interface definition

(define-generics collection
  ;; This interface has the following groups of methods:
  ;;  - structural traversal (a la gen:stream, for list-likes)
  ;;  - stateful traversal (a la iterator pattern, for vector-likes)
  ;;    uses an auxiliary struct that implements `gen:iterator'
  ;;    TODO is there a use for a functional version of `gen:iterator'
  ;;     that would return a fresh iterator every call to `next'?
  ;;  - derived traversals (fold and co. with fallbacks using either of the
  ;;    above groups of methods)
  ;;  - structural building (a la empty+cons)
  ;;  - stateful building (with an explicit constructor)
  ;;    uses an auxiliary struct that implements `gen:builder'
  ;;    TODO is there a use for a functional version of `gen:builder'?
  ;;    TODO same as stateful traversals, maybe have an auxiliary builder
  ;;     structure (that implements a gen:builder) and call *its* methods
  ;;  - derived building (unfold, range and co. again with fallbacks)
  ;;  - "transducers" (for lack of a better name) (map and co. need both
  ;;    a way to traverse and a way to build. again with fallbacks)
  ;;  TODO also a group for in-place (mutation-based) building? or it that
  ;;   covered by stateful building? (I guess order wouldn't be defined, which
  ;;   is a problem for in-place changes. still, can override methods that
  ;;   traverse in wrong order)


  ;; Structural traversal
  [empty? collection]
  [first collection]
  [rest collection]
  
  ;; Stateful traversal
  [make-iterator collection] ; returns a gen:iterator
  
  ;; Derived traversals
  [length collection]
  [foldr f base collection . cs]
  [foldl f base collection . cs]
  ;; TODO others

  ;; Structural building
  [make-empty collection] ; returns a new empty coll. (think `(λ (l) '())')
  [cons x collection]
  ;; TODO alternative API: `make-empty' and `get-cons' (returns cons)

  ;; Stateful building
  [make-builder collection] ; returns a gen:builder

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
  [map f collection . cs]
  [filter f collection]
  ;; TODO others

  #:defined-predicate collection-implements?
  #:fallbacks
  [
   ;; Derived traversals, depend on either kind of basic traversals
   (define length fallback-length)
   (define foldr  fallback-foldr)
   (define foldl  fallback-foldl)

   ;; Derived buildings, depend on either kind of basic building
   (define range  fallback-range)
   (define make   fallback-make)
   (define build  fallback-build)

   ;; Derived transducers, need both a way to traverse and a way to build
   (define map    fallback-map)
   (define filter fallback-filter)
   ]
  ;; TODO add defaults (lists, vectors, etc.)
  )

;; Interface taken from Java / Scala
(define-generics iterator
  [has-next? iterator]
  [next iterator])

(define-generics builder
  [add-next x builder]
  [finalize builder]) ; returns a gen:collection
;; TODO this only builds "in order", but order is defined by the builder
;; TODO more efficient building (e.g. if we know the size in advance) can
;;  be done by overriding derived methods. can we do better?



;;;---------------------------------------------------------------------------
;;; Tests

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

    (check-equal? (map + (kons-list '(1 2 3 4)) (kons-list '(2 3 4 5)))
                  (kons-list '(3 5 7 9)))
    (check-exn exn:fail:contract:arity?
               (lambda ()
                 (filter (lambda (x y) (< 5 (+ x y)))
                         (kons-list '(1 2 3 4)) (kons-list '(2 3 4 5)))))

    (check-equal? (foldr list 'x
                         (range (kons-list '()) 3)
                         (kons-list '(5 6 7))
                         (kons-list '(11 12 13)))
                  '(0 5 11 (1 6 12 (2 7 13 x))))
    (check-equal? (foldl list 'x
                         (range (kons-list '()) 3)
                         (kons-list '(5 6 7))
                         (kons-list '(11 12 13)))
                  '(2 7 13 (1 6 12 (0 5 11 x))))
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
         (define (foldl f b k . cs)
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

    (check-equal? (map + (kons-list/length 4 '(1 2 3 4))
                       (kons-list/length 4 '(2 3 4 5)))
                  (kons-list/length 4 '(3 5 7 9)))
    ;; heterogeneous case, first collection type wins
    (check-equal? (map + (kons-list/length 4 '(1 2 3 4))
                       (kons-list '(2 3 4 5)))
                  (kons-list/length 4 '(3 5 7 9)))
    ))

(struct not-really-a-collection ()
        #:methods gen:collection
        [])

(module+ test
  (check-exn exn:fail? (lambda () (length 'not-a-collection)))
  (check-exn exn:fail? (lambda () (length (not-really-a-collection)))))

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
           (vektor (list->vector (reverse (vektor-builder-l v)))))])

(module+ test
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

    (check-equal? (range mt 4) (vektor '#(0 1 2 3)))
    (check-equal? (make mt 4 'a) (vektor '#(a a a a)))
    (check-equal? (build mt 4 add1) (vektor '#(1 2 3 4)))

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
                         (range (vektor '#()) 3)
                         (kons-list '(5 6 7))
                         (vektor '#(11 12 13)))
                  '(0 5 11 (1 6 12 (2 7 13 x))))
    (check-equal? (foldl list 'x
                         (range (vektor '#()) 3)
                         (kons-list '(5 6 7))
                         (vektor '#(11 12 13)))
                  '(2 7 13 (1 6 12 (0 5 11 x))))
    ))
