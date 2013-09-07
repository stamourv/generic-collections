#lang racket

;;; Interface definition (and "documentation") below.

(require racket/stxparam
         (for-syntax racket/syntax))

;; TODO add exports

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


(define-syntax-rule (with-structural-traversal (c) (extra-acc ...) body)
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
     body)))

(define-syntax-rule (with-stateful-traversal (c) (extra-acc ...) body)
  (let ([acc (make-iterator c)])
    (let loop (extra-acc ...)
      (syntax-parameterize
       ([-loop
         (...
          (syntax-rules ()
            [(_ in extra-arg ...)
             ;; No need to pass acc around, but can't drop it either (can be
             ;; side-effectful, like `-rest!'). If it's just `-rest', should be
             ;; compiled away anyway.
             (begin in (loop extra-arg ...))]))]
        [-empty?
         (syntax-rules () [(_) (not (has-next? acc))])]
        [-first
         (syntax-rules () [(_) (next acc)])]
        [-rest
         (syntax-rules () [(_) acc])]
        [-rest!
         (syntax-rules () [(_) (begin (next acc) acc)])])
       body))))

(define-syntax-rule (with-n-ary-traversal (colls) (extra-acc ...) body)
  (let ()
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
       body))))

(define (mt? it s?) (if s? (empty? it) (not (has-next? it))))

;; placeholder for the collection argument(s)
(define-syntax -coll (syntax-rules ()))

(define-syntax (traversal stx)
  (syntax-case stx ()
    [(_ (maybe-coll-args ...) (extra-acc ...)
        body-1-coll
        body-n-colls)
     (let ()
       (define single-collection-part ; just missing the `lambda'
         ;; TODO transducer may need something similar
         (with-syntax* ([c (generate-temporary 'c)]
                        [(args ...)
                         (for/list ([a (in-list (syntax->list
                                                 #'(maybe-coll-args ...)))])
                           ;; replace placeholder
                           (if (and (identifier? a) ; can be a kw or optional
                                    (free-identifier=? a #'-coll))
                               #'c
                               a))])
           (syntax/loc stx
             ((args ...)
              (cond
               [(can-do-structural-traversal? c)
                (with-structural-traversal
                 (c) (extra-acc ...)
                 body-1-coll)]
               [(can-do-stateful-traversal? c)
                (with-stateful-traversal
                 (c) (extra-acc ...)
                 body-1-coll)]
               [else
                (error "cannot traverse collection" c)])))))

       (define multi-collection-part
         (with-syntax ([(args ...)
                        ;; for n-ary case, collections have to go at the end,
                        ;; so drop the placeholder
                        (for/list ([a (in-list (syntax->list
                                                #'(maybe-coll-args ...)))]
                                   #:unless
                                   (and (identifier? a)
                                        (free-identifier=? a #'-coll)))
                          a)])
           (syntax/loc stx
             ((args ... c . cs)
              (define colls (r:cons c cs))
              (unless (for/and ([coll (in-list colls)])
                        (or (can-do-structural-traversal? coll)
                            can-do-stateful-traversal? coll))
                ;; TODO have better error message than that
                (error "cannot traverse one of" colls))
              (with-n-ary-traversal
               (colls) (extra-acc ...)
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

(define fallback-foldr
  (traversal
   (f base -coll) ()
   (if (-empty?)
       base
       (f (-first) (-loop (-rest))))
   (if (-empty?)
       base
       ;; TODO can I do it without the append?
       (apply f (r:append (-first) (list (-loop (-rest))))))))

(define fallback-length
  (traversal
   (-coll) ([n 0])
   (if (-empty?)
       n
       (-loop (-rest!) (add1 n)))
   #f))

(define fallback-foldl
  (traversal
   (f base -coll) ([acc base])
   (if (-empty?)
       acc
       (-loop (-rest) (f (-first) acc)))
   (if (-empty?)
       acc
       ;; TODO can I do it without the append?
       (-loop (-rest) (apply f (r:append (-first) (list acc)))))))

(define fallback-andmap
  (traversal
   (f -coll) ()
   (if (-empty?)
       #t
       (and (f (-first)) (-loop (-rest))))
   (if (-empty?)
       #t
       (and (apply f (-first)) (-loop (-rest))))))

(define fallback-ormap
  (traversal
   (f -coll) ()
   (if (-empty?)
       #f
       (or (f (-first)) (-loop (-rest))))
   (if (-empty?)
       #f
       (or (apply f (-first)) (-loop (-rest))))))

(define fallback-ref
  (traversal
   (-coll i) ([i i])
   (cond [(-empty?)
          (error "index out of range" i)]
         [(= i 0)
          (-first)]
         [else
          (-loop (-rest!) (sub1 i))])
   #f))

(define fallback-for-each
  (traversal
   (f -coll) ()
   (if (-empty?)
       (void)
       (begin (f (-first))
              (-loop (-rest))))
   (if (-empty?)
       (void)
       (begin (apply f (-first))
              (-loop (-rest))))))

(define member?-error-thunk (lambda () (error "member?: element not found")))
(define fallback-member?
  (traversal
   (x -coll #:equal? [=? equal?] [fail member?-error-thunk]) ()
   (if (-empty?)
       (if (procedure? fail) (fail) fail)
       (let ([y (-first)])
         (if (=? x y)
             y
             (-loop (-rest)))))
   #f))


(define (can-do-structural-building? c)
  (and (collection-implements? c 'make-empty)
       (collection-implements? c 'cons)))

(define (can-do-stateful-building? c)
  (collection-implements? c 'make-builder))


(define-syntax-parameter -base (syntax-rules ()))

(define-syntax-rule (with-structural-building (c) body)
  (syntax-parameterize
   ([-base (syntax-rules () [(_) (make-empty c)])])
   body))

(define-syntax-rule (with-stateful-building (c) body)
  (let ([builder (make-builder c)])
    (syntax-parameterize
     ([-base (syntax-rules () [(_) builder])])
     (begin body (finalize builder)))))

(define-syntax (define-builder stx)
  (syntax-case stx ()
    [(_ [name export-name fallback-name default-name]
        body)
     (quasisyntax/loc stx
       (begin
         (provide (rename-out [export-name name]))
         (define fallback-name
           body)
         (define (export-name #:collection [c #f] . args)
           ;; TODO raise proper arity errors
           (if c
               (apply name c args)
               (apply default-name args)))))]))

(define-syntax (building stx)
  (syntax-case stx ()
    [(_ (non-coll-args ...)
        (extra-acc-structural ...) (extra-acc-stateful ...)
        body-structural
        body-stateful)
     (quasisyntax/loc stx
       (lambda (c non-coll-args ...)
         (cond
          [(can-do-structural-building? c)
           (with-structural-building
            (c)
            (let loop (extra-acc-structural ...)
              (syntax-parameterize
               ([-loop (make-rename-transformer #'loop)])
               body-structural)))]
          [(can-do-stateful-building? c)
           (with-stateful-building
            (c)
            (let loop (extra-acc-stateful ...)
              (syntax-parameterize
               ([-loop (make-rename-transformer #'loop)])
               body-stateful)))]
          [else
           (error "cannot build collection" c)])))]))

;; User-facing `range', only optionally takes a collection to dispatch on,
;; defaults to lists. Overriding methods have a different signature, which
;; may be confusing. Document adequately.
(define-builder [range range/export fallback-range l:range]
  (case-lambda
    [(c end)
     ;; Use the fallback so that clients of range don't have to know
     ;; whether build was overriden.
     (fallback-build c end values)]
    [(c start end)
     (fallback-build c (- end start) (lambda (x) (+ x start)))]
    [(c start end step)
     (fallback-build c
                     (exact-ceiling (/ (- end start) step))
                     (lambda (x) (+ (* x step) start)))]))

(define-builder [make make/export fallback-make make-list]
  (building
   (n v) ([i n] [acc (-base)]) ([i 0])
   (if (= i 0)
       acc
       (-loop (sub1 i) (cons v acc)))
   (when (< i n)
     (add-next v (-base))
     (-loop (add1 i)))))

(define-builder [build build/export fallback-build build-list]
  (building
   (n f) ([i (sub1 n)] [acc (-base)]) ([i 0])
   (if (< i 0)
       acc
       (-loop (sub1 i) (cons (f i) acc)))
   (when (< i n)
     (add-next (f i) (-base))
     (-loop (add1 i)))))


(define-syntax (transducer stx)
  (syntax-case stx ()
    [(_ (non-coll-args ...)
        (extra-acc-structural ...) (extra-acc-stateful ...)
        body-1-coll-structural
        body-1-coll-stateful
        body-n-colls-structural
        body-n-colls-stateful)
     (let ()
       (define single-collection-part ; just missing the `lambda'
         (syntax/loc stx
           ((non-coll-args ... c)
            (cond
             [(and (can-do-structural-traversal? c)
                   (can-do-structural-building? c))
              (with-structural-building
               (c)
               (with-structural-traversal
                (c) (extra-acc-structural ...)
                body-1-coll-structural))]
             [(and (can-do-stateful-traversal? c)
                   (can-do-stateful-building? c))
              (let ([acc (make-iterator c)])
                (with-stateful-building
                 (c)
                 (with-stateful-traversal
                  (c) (extra-acc-stateful ...)
                  body-1-coll-stateful)))]
             [else
              ;; TODO are the structural / stateful combinations interesting?
              (unless (or (can-do-structural-traversal? c)
                          (can-do-stateful-traversal? c))
                (error "cannot traverse collection" c))
              (unless (or (can-do-structural-building? c)
                          (can-do-stateful-building? c))
                (error "cannot build collection" c))]))))

       (define multi-collection-part
         (quasisyntax/loc stx
           ((non-coll-args ... c . cs)
            (define colls (r:cons c cs))
            ;; TODO are the structural / stateful combinations interesting?
            (unless (for/and ([coll (in-list colls)])
                      (or (and (can-do-structural-traversal? coll)
                               (can-do-structural-building? coll))
                          (and (can-do-stateful-traversal? coll)
                               (can-do-stateful-building? coll))))
              ;; TODO have better error message than that
              (error "cannot traverse or build one of" colls))
            (if (can-do-structural-building? c)
                (with-structural-building
                 (c)
                 (with-n-ary-traversal
                  (colls) (extra-acc-structural ...)
                  body-n-colls-structural))
                (with-stateful-building
                 (c)
                 (with-n-ary-traversal
                  (colls) (extra-acc-stateful ...)
                  body-n-colls-stateful))))))

       (cond
        [(and (syntax->datum #'body-1-coll-structural)
              (syntax->datum #'body-1-coll-stateful)
              (syntax->datum #'body-n-colls-structural)
              (syntax->datum #'body-n-colls-stateful))
         (quasisyntax/loc stx
           (case-lambda
             [#,@single-collection-part]
             [#,@multi-collection-part]))]
        [(and (syntax->datum #'body-1-coll-structural)
              (syntax->datum #'body-1-coll-stateful))
         (quasisyntax/loc stx (lambda #,@single-collection-part))]
        [(and (syntax->datum #'body-n-colls-structural)
              (syntax->datum #'body-n-colls-stateful))
         (quasisyntax/loc stx (lambda #,@multi-collection-part))]
        [else
         (raise-syntax-error
          'traversal "bad body specification" stx)]))]))

(define fallback-map
  (transducer
   (f) () ()
   (if (-empty?)
       (-base)
       (cons (f (-first)) (-loop (-rest))))
   (if (-empty?)
       'done
       (begin (add-next (f (-first)) (-base))
              (-loop (-rest))))
   (if (-empty?)
       (-base)
       (cons (apply f (-first)) (-loop (-rest))))
   (if (-empty?)
       'done
       (begin (add-next (apply f (-first)) (-base))
              (-loop (-rest))))))

(define fallback-filter
  (transducer
   (f) () ()
   (if (-empty?)
       (-base)
       (let ([elt (-first)])
         (if (f elt)
             (cons elt (-loop (-rest)))
             (-loop (-rest)))))
   (if (-empty?)
       'done
       (let ([elt (-first)])
         (when (f elt)
           (add-next elt (-base)))
         (-loop (-rest))))
   #f
   #f))

(define fallback-reverse
  (transducer
   () ([acc (-base)]) ()
   (if (-empty?)
       acc
       (-loop (-rest) (cons (-first) acc)))
   (if (-empty?)
       'done
       (let ([elt (-first)])
         (define res
           (begin0 (-loop (-rest))
           (add-next elt (-base))))
         res))
   #f
   #f))

;; The `transducer' macro is good for lock-step traversals, so doesn't
;; work here.
(define (fallback-append c . cs)
  (cond [(empty? cs) ; nothing to append
         c]
        [(can-do-structural-building? c)
         (let loop ([rev-acc (make-empty c)]
                    [cs      (r:cons c cs)])
           (cond [(empty? cs)
                  (reverse rev-acc)]
                 [else
                  (loop (structural-append2 (l:first cs) rev-acc)
                        (l:rest cs))]))]
        [(can-do-stateful-building? c)
         (define builder (make-builder c))
         (let loop ([cs (r:cons c cs)])
           (cond [(empty? cs)
                  (finalize builder)]
                 [else
                  (stateful-append2 (l:first cs) builder)
                  (loop (l:rest cs))]))]
        [else
         (error "cannot build collection" c)]))
(define structural-append2
  (traversal
   (-coll rev-acc) ([rev-acc rev-acc])
   (if (-empty?)
       rev-acc
       (-loop (-rest) (cons (-first) rev-acc)))
   #f))
(define stateful-append2
  (traversal
   (-coll builder) ()
   (if (-empty?)
       (void)
       (begin (add-next (-first) builder)
              (-loop (-rest))))
   #f))

(define fallback-remove
  (transducer
   (x #:equal? [= equal?]) () ([found-yet? #f])
   (if (-empty?)
       (-base)
       (let ([head (-first)])
         (if (= head x)
             (-rest)
             (cons head (-loop (-rest))))))
   (if (-empty?)
       'done
       (let* ([head   (-first)]
              [found? (= head x)])
         (unless (and found? (not found-yet?))
           (add-next head (-base)))
         (-loop (-rest) (or found-yet? found?))))
   #f
   #f))

(define fallback-remove*
  (transducer
   (x #:equal? [= equal?]) () ()
   (if (-empty?)
       (-base)
       (let ([head (-first)])
         (if (= head x)
             (-loop (-rest))
             (cons head (-loop (-rest))))))
   (if (-empty?)
       'done
       (let ([head (-first)])
         (unless (= head x)
           (add-next head (-base)))
         (-loop (-rest))))
   #f
   #f))


;;;---------------------------------------------------------------------------
;;; Interface definitions

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
  ;;  TODO add imperative updates (maybe just a set-at! method + derived?)
  ;;    in which case, also add a copy operation

  ;; Operations that that n collections as arguments must accept
  ;; heterogeneous inputs (i.e. most work when given, e.g. a list and a
  ;; vector).


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
  [andmap f collection . cs]
  [ormap  f collection . cs]
  ;; This one makes a lot of sense to override for vector-like things.
  ;; Some derived methods may want to use this instead of iterators.
  [ref collection i]
  [for-each f collection . cs]
  ;; TODO leave member? to sets?
  [member? x collection #:equal? [=] [failure-result/thunk]]

  ;; Structural building
  [make-empty collection] ; returns a new empty coll. (think `(λ (l) '())')
  [cons x collection]
  ;; TODO alternative API: `make-empty' and `get-cons' (returns cons)

  ;; Stateful building
  [make-builder collection] ; returns a gen:builder

  ;; Derived building
  ;; These need a collection to dispatch on, which is a bit clunky.
  ;; User-facing versions do not and default to lists. That does mean
  ;; that the main function and methods have different signatures, which
  ;; may be confusing.
  [range collection x [y] [z]]
  [make  collection n v] ; think make-list
  [build collection n f] ; think build-list
  ;; TODO test overrides for these

  ;; Transducers
  [map f collection . cs]
  [filter f collection]
  [reverse collection]
  [append collection . cs]
  ;; not exactly like current `remove', but consistent with `member?'
  [remove  x collection #:equal? [=]]
  [remove* x collection #:equal? [=]]

  ;; TODO other operations (from racket/base, racket/list, racket/string
  ;; racket/vector, srfi/1, srfi/43, unstable/list and others):

  ;; sort (use r:sort + ->list and list->), second, third and co, last,
  ;; take, drop, split-at, takef, dropf, splitf-at, take-right,
  ;; drop-right, split-at-right, takef-right, dropf-right,
  ;; splitf-at-right, add-between, append*, flatten, remove-duplicates,
  ;; filter-map, count, partition, append-map, filter-not, shuffle,
  ;; permutations, in-permutations, argmin, argmax, ->list, list->,
  ;; string-trim, string-replace, string-split, string-join,
  ;; vector-copy, list-prefix?, take-common-prefix, drop-common-prefix,
  ;; split-common-prefix, filter-multiple, extend, check-duplicate,
  ;; group-by (change interface as discussed with eli), list-update,
  ;; list-set, slice (like in-slice), cons* / list*, zip, unzip[1..5],
  ;; unfold, unfold-right, list-index, list-index-right, substring,
  ;; string-pad (avoid string-pad-right in the same way as
  ;; racket/string's string-trim), compare (like string<? and co, but
  ;; takes a comparison procedure, like sort), sliding window, convolve,
  ;; rotate

  ;; These would require an in-place update method:
  ;; string-fill!, vector-copy!, vector-set*!, vector-map!, take!, drop!
  ;; (and others in that family), append!, append*!, reverse!,
  ;; append-map!, filter!, partition!, remove!


  #:defined-predicate collection-implements?
  #:fallbacks
  [
   ;; Derived traversals, depend on either kind of basic traversals
   (define length   fallback-length)
   (define foldr    fallback-foldr)
   (define foldl    fallback-foldl)
   (define andmap   fallback-andmap)
   (define ormap    fallback-ormap)
   (define ref      fallback-ref)
   (define for-each fallback-for-each)
   (define member?  fallback-member?)

   ;; Derived buildings, depend on either kind of basic building
   (define range  fallback-range)
   (define make   fallback-make)
   (define build  fallback-build)

   ;; Derived transducers, need both a way to traverse and a way to build
   (define map     fallback-map)
   (define filter  fallback-filter)
   (define reverse fallback-reverse)
   (define append  fallback-append)
   (define remove  fallback-remove)
   (define remove* fallback-remove*)
   ]

  
  #:fast-defaults
  ([list?
    (define empty? l:empty?)
    (define first  l:first)
    (define rest   l:rest)
    ;; no stateful traversal
    (define length r:length)
    (define (foldr f base . ls)
      (if (r:andmap list? ls)
          (apply r:foldr f base ls) ; homogeneous case
          ;; heterogeneous case, use fallback
          (apply fallback-foldr f base ls)))
    (define (foldl f base . ls)
      (if (r:andmap list? ls)
          (apply r:foldl f base ls) ; homogeneous case
          ;; heterogeneous case, use fallback
          (apply fallback-foldl f base ls)))
    (define (andmap f . ls)
      (if (r:andmap list? ls)
          (apply r:andmap f ls) ; homogeneous case
          ;; heterogeneous case, use fallback
          (apply fallback-andmap f ls)))
    (define (ormap f . ls)
      (if (r:andmap list? ls)
          (apply r:ormap f ls) ; homogeneous case
          ;; heterogeneous case, use fallback
          (apply fallback-ormap f ls)))
    (define ref      r:list-ref)
    (define for-each r:for-each)
    ;; stock member is not the same
    (define (member? x l #:equal? [=? equal?] [fail member?-error-thunk])
      (define res (r:member x l =?))
      (cond [res               (l:first res)]
            [(procedure? fail) (fail)]
            [else              fail]))
    (define (make-empty _)
      l:empty)
    (define cons     r:cons)
    ;; no stateful building
    (define range l:range)
    (define make  make-list)
    (define build build-list)
    (define (map f . ls)
      (if (r:andmap list? ls)
          (apply r:map f ls) ; homogeneous case
          ;; heterogeneous case, use fallback
          (apply fallback-map f ls)))
    (define filter  r:filter)
    (define reverse r:reverse)
    (define append (lambda ls
                     (if (r:andmap list? ls)
                         (apply r:append ls)
                         (apply fallback-append ls))))
    (define (revove x l #:equal? [= equal?])
      (r:remove x l =))
    (define (remove* x l #:equal? [= equal?])
      (r:remove* x l =))
    ])

  ;; TODO add more defaults (hashes, strings, bytes, ports?, sequences,
  ;;  streams, uniform vectors, math/array, dicts, sets, mlists,
  ;;  integers?, etc.)
  #:defaults
  ([vector?
    ;; no structural traversal
    (struct vector-iterator (v i l) #:mutable
            #:methods gen:iterator
            [(define (has-next? v)
               (< (vector-iterator-i v) (vector-iterator-l v)))
             (define (next v)
               (begin0 (vector-ref (vector-iterator-v v) (vector-iterator-i v))
                 (set-vector-iterator-i! v (add1 (vector-iterator-i v)))))])
    (define (make-iterator v) (vector-iterator v 0 (vector-length v)))
    (define length   vector-length)
    (define ref      vector-ref)
    ;; no structural building
    (struct vector-builder (l) #:mutable #:transparent
            #:methods gen:builder
            [(define (add-next x v)
               (set-vector-builder-l! v (r:cons x (vector-builder-l v))))
             (define (finalize v)
               (list->vector (r:reverse (vector-builder-l v))))])
    (define (make-builder _) (vector-builder '()))
    (define make   make-vector)
    (define build  build-vector)
    (define map    (lambda (f . ls)
                     (if (r:andmap vector? ls)
                         (apply vector-map f ls) ; homogeneous case
                         ;; heterogeneous case, use fallback
                         (apply fallback-map f ls))))
    (define filter vector-filter)
    (define append (lambda vs
                     (if (r:andmap vector? vs)
                         (apply vector-append vs)
                         (apply fallback-append vs))))
    ])
  )


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

;; Not a useful function, just an additional test for the transducer macro.
(define n-ary-reverse
  (transducer
   () ([acc (-base)]) ()
   #f
   #f
   (if (-empty?)
       acc
       (-loop (-rest) (cons (-first) acc)))
   (if (-empty?)
       'done
       (let ([elt (-first)])
         (define res
           (begin0 (-loop (-rest))
           (add-next elt (-base))))
         res))))

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
    
    (check-equal? (reverse (kons-list '(1 2 3))) (kons-list '(3 2 1)))
    (check-equal? (n-ary-reverse (kons-list '(1 2 3)) (kons-list '(4 5 6)))
                  (kons-list '((3 6) (2 5) (1 4))))

    ;; taken from range's test suite
    (check-equal? (range mt 4) (kons-list '(0 1 2 3)))
    (check-equal? (range mt 0) (kons-list '()))
    (check-equal? (range mt 8) (kons-list '(0 1 2 3 4 5 6 7)))
    (check-equal? (range mt 3 2) (kons-list '()))
    (check-equal? (range mt 3 2 -1) (kons-list '(3)))
    (check-equal? (range mt 3 9) (kons-list '(3 4 5 6 7 8)))
    (check-equal? (range mt 3 9 2) (kons-list '(3 5 7)))
    (check-equal? (range mt 3 9 0.5)
                  (kons-list '(3 3.5 4.0 4.5 5.0 5.5 6.0 6.5 7.0 7.5 8.0 8.5)))
    (check-equal? (range mt 9 3 -2) (kons-list '(9 7 5)))
    (check-equal? (range mt 10) (kons-list '(0 1 2 3 4 5 6 7 8 9)))
    (check-equal? (range mt 10 20)
                  (kons-list '(10 11 12 13 14 15 16 17 18 19)))
    (check-equal? (range mt 20 40 2)
                  (kons-list '(20 22 24 26 28 30 32 34 36 38)))
    (check-equal? (range mt 20 10 -1)
                  (kons-list '(20 19 18 17 16 15 14 13 12 11)))
    (check-equal? (range mt 10 15 1.5)
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

    (check-equal? (reverse (kons-list/length 3 '(1 2 3)))
                  (kons-list/length 3 '(3 2 1)))
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
           (vektor (list->vector (r:reverse (vektor-builder-l v)))))])

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

    (check-equal? (reverse (vektor '#(1 2 3))) (vektor '#(3 2 1)))
    (check-equal? (n-ary-reverse (vektor '#(1 2 3)) (kons-list '(4 5 6)))
                  (vektor '#((3 6) (2 5) (1 4))))

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
    ))

(struct range-struct (min max) #:transparent
        #:methods gen:collection
        ;; make sure that this is overridden, even when calling range/export
        [(define/generic generic-build build)
         (define (range c n [m #f] [step #f])
           ;; not a complete or correct implementation
           (generic-build (kons-list '())
                          (- (range-struct-max c) (range-struct-min c))
                          (lambda (x) (+ x (range-struct-min c)))))])

(module+ test
  (let ()

    ;; tests for optional dispatch for builders
    
    (check-equal? (range (range-struct 4 6) #f) (kons-list '(4 5)))
    (check-equal? (range/export #:collection (range-struct 4 6) #f)
                  (kons-list '(4 5)))
    (check-equal? (range/export 4) '(0 1 2 3))
    (check-equal? (range/export 4 6) '(4 5))
    (check-equal? (range/export 4 -2 -2) '(4 2 0))
    (check-equal? (range/export #:collection (kons-list '()) 4)
                  (kons-list '(0 1 2 3)))
    (check-equal? (range/export #:collection (kons-list '()) 4 6)
                  (kons-list '(4 5)))
    (check-equal? (range/export #:collection (kons-list '()) 4 -2 -2)
                  (kons-list '(4 2 0)))

    (check-equal? (make/export 5 5) '(5 5 5 5 5))
    (check-equal? (make/export #:collection (kons-list '()) 5 5)
                  (kons-list '(5 5 5 5 5)))
    (check-equal? (build/export 5 values) '(0 1 2 3 4))
    (check-equal? (build/export #:collection (kons-list '()) 5 values)
                  (kons-list '(0 1 2 3 4)))

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
    (check-equal? (range '#() 1 10 2) '#(1 3 5 7 9))

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

    ))
