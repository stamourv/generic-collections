#lang racket

;;; Interface definition (and "documentation") below.

(require racket/stxparam
         (for-syntax racket/syntax))

(provide gen:collection collection?
         gen:iterator iterator? has-next? next
         gen:builder builder? add-next finalize

         empty? first rest
         make-iterator
         length foldr foldl andmap ormap
         ref first second third fourth fifth sixth seventh eighth ninth last
         for-each member?

         make-empty (rename-out [cons/export cons])
         make-builder
         range make build

         map filter reverse append remove remove*
         take drop split-at take-right drop-right split-at-right
         )

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
       (syntax-rules () [(_) (rest acc)])]
      [-coll
       (make-rename-transformer #'acc)])
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
         (syntax-rules () [(_) (begin (next acc) acc)])]
        [-coll
         (make-rename-transformer #'acc)])
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
;; should always be put at the end for n-ary operations (otherwise the
;; 1-collection and n-collection versions will differ in argument order)
;; for 1-collection bodies, bound to the collection itself (so far in the loop)
(define-syntax-parameter -coll (syntax-rules ()))

(define-syntax (traversal stx)
  (syntax-case stx ()
    [(_ (maybe-coll-args ...) (extra-acc ...)
        body-1-coll
        body-n-colls)
     (let ()
       (define single-collection-part ; just missing the `lambda'
         ;; TODO abstract with transducer
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

(define (fallback-second c)
  (ref c 1))
(define (fallback-third c)
  (ref c 2))
(define (fallback-fourth c)
  (ref c 3))
(define (fallback-fifth c)
  (ref c 4))
(define (fallback-sixth c)
  (ref c 5))
(define (fallback-seventh c)
  (ref c 6))
(define (fallback-eighth c)
  (ref c 7))
(define (fallback-ninth c)
  (ref c 8))
(define (fallback-tenth c)
  (ref c 9))

;; Doesn't really fit the `traversal' pattern (needs lookahead).
(define (fallback-last c)
  (cond [(can-do-structural-traversal? c)
         (if (empty? c)
             (error "last of empty collection" c)
             (let loop ([c (rest c)] [prev (first c)])
               (if (empty? c)
                   prev
                   (loop (rest c) (first c)))))]
        [(can-do-stateful-traversal? c)
         (define it (make-iterator c))
         (if (has-next? it)
             (let loop ([prev (next it)])
               (if (has-next? it)
                   (loop (next it))
                   prev))
             (error "last of empty collection" c))]
        [else
         (error "cannot traverse collection" c)]))

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
    [(_ [name fallback-name method-name default-name]
        body)
     (quasisyntax/loc stx
       (begin
         (define fallback-name
           body)
         (define (name #:collection [c #f] . args)
           ;; TODO raise proper arity errors
           (if c
               (apply method-name c args)
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
(define-builder [range fallback-range range-method l:range]
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

(define-builder [make fallback-make make-method make-list]
  (building
   (n v) ([i n] [acc (-base)]) ([i 0])
   (if (= i 0)
       acc
       (-loop (sub1 i) (cons v acc)))
   (when (< i n)
     (add-next v (-base))
     (-loop (add1 i)))))

(define-builder [build fallback-build build-method build-list]
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
    [(_ (maybe-coll-args ...)
        (extra-acc-structural ...) (extra-acc-stateful ...)
        body-1-coll-structural
        body-1-coll-stateful
        body-n-colls-structural
        body-n-colls-stateful)
     (let ()
       (define single-collection-part ; just missing the `lambda'
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
                  (error "cannot build collection" c))])))))

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
           (quasisyntax/loc stx
             ((args ... c . cs)
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
                    body-n-colls-stateful)))))))

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
   (f -coll) () ()
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
   (f -coll) () ()
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
   (-coll) ([acc (-base)]) ()
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
   (x #:equal? [= equal?] -coll) () ([found-yet? #f])
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
   (x #:equal? [= equal?] -coll) () ()
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

(define fallback-take
  (transducer
   (-coll n) ([i n] [rev-acc (-base)]) ([i n])
   (cond [(= i 0)
          (reverse rev-acc)]
         [(-empty?)
          (error "take: index is too large for list" n)]
         [else
          (-loop (-rest) (sub1 i) (cons (-first) rev-acc))])
   (cond [(= i 0)
          (finalize (-base))]
         [(-empty?)
          (error "take: index is too large for list" n)]
         [else
          (add-next (-first) (-base))
          (-loop (-rest) (sub1 i))])
   #f
   #f))
(define fallback-drop
  (transducer
   (-coll n) ([i n]) ([i n])
   (cond [(= i 0) ; nothing to drop, return what's left
          -coll]
         [(-empty?)
          (error "drop: index is too large for list" n)]
         [else
          (-loop (-rest) (sub1 i))])
   (cond [(= i 0) ; nothing to drop, start building
          (unless (-empty?)
            (add-next (-first) (-base))
            (-loop (-rest) i))]
         [(-empty?)
          (error "drop: index is too large for list" n)]
         [else
          (-loop (-rest!) (sub1 i))])
   #f
   #f))
;; can't be a transducer, since those return a single value
(define (fallback-split-at coll n)
  (cond
   [(and (can-do-structural-traversal? coll)
         (can-do-structural-building?  coll))
    (let loop ([coll coll] [rev-acc (make-empty coll)] [i n])
      (cond [(= i 0)
             (values (reverse rev-acc) coll)]
            [(empty? coll)
             (error "split-at: index is too large for list" n)]
            [else
             (loop (rest coll) (cons (first coll) rev-acc) (sub1 i))]))]
   [(and (can-do-stateful-traversal? coll)
         (can-do-stateful-building?  coll))
    (define it (make-iterator coll))
    (define first-half  (make-builder coll))
    (define second-half (make-builder coll))
    (let loop ([i n])
      (cond [(= i 0)
             (let inner ()
               (when (has-next? it)
                 (add-next (next it) second-half)
                 (inner)))]
            [(has-next? it)
             (add-next (next it) first-half)
             (loop (sub1 i))]
            [else
             (error "split-at: index is too large for list" n)]))
    (values (finalize first-half) (finalize second-half))]
   ;; TODO do the struct/state and state/struct combinations?
   [else
    (error "cannot traverse or build collection" coll)]))

(define (fallback-take-right coll n) ; TODO add error checks for negatives
  ;; Or could call the fallbacks directly, to not change behavior if `take'
  ;; is overridden.
  (drop coll (- (length coll) n)))
(define (fallback-drop-right coll n)
  (take coll (- (length coll) n)))
(define (fallback-split-at-right coll n)
  (split-at coll (- (length coll) n)))


;; The `cons' method can only cons on top of collections, so it can't be used
;; to create plain pairs. Need to wrap it to allow that.
;; In the future, maybe `cons' should have the behavior from the student langs
;; and improper lists should disappear.
(define (cons/export a r)
  (if (collection? r)
      (cons a r)
      (r:cons a r)))

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
  [ref     collection i]
  [second  collection]
  [third   collection]
  [fourth  collection]
  [fifth   collection]
  [sixth   collection]
  [seventh collection]
  [eighth  collection]
  [ninth   collection]
  [last    collection]
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
  [range-method collection x [y] [z]]
  [make-method  collection n v] ; think make-list
  [build-method collection n f] ; think build-list
  ;; TODO test overrides for these

  ;; Transducers
  [map f collection . cs]
  [filter f collection]
  [reverse collection]
  [append collection . cs]
  ;; not exactly like current `remove', but consistent with `member?'
  [remove  x collection #:equal? [=]]
  [remove* x collection #:equal? [=]]
  [take     collection n]
  [drop     collection n]
  [split-at collection n]
  [take-right     collection n]
  [drop-right     collection n]
  [split-at-right collection n]

  ;; TODO other operations (from racket/base, racket/list, racket/string
  ;; racket/vector, srfi/1, srfi/43, unstable/list and others):

  ;; sort (use r:sort + ->list and list->), takef, dropf, splitf-at,
  ;; takef-right, dropf-right, splitf-at-right, add-between, append*,
  ;; flatten, remove-duplicates, filter-map, count, partition,
  ;; append-map, filter-not, shuffle, permutations, in-permutations,
  ;; argmin, argmax, ->list, list->, string-trim, string-replace,
  ;; string-split, string-join, vector-copy, list-prefix?,
  ;; take-common-prefix, drop-common-prefix, split-common-prefix,
  ;; filter-multiple, extend, check-duplicate, group-by (change
  ;; interface as discussed with eli), list-update, list-set, slice
  ;; (like in-slice), cons* / list*, zip, unzip[1..5], unfold,
  ;; unfold-right, list-index, list-index-right, substring, string-pad
  ;; (avoid string-pad-right in the same way as racket/string's
  ;; string-trim), compare (like string<? and co, but takes a comparison
  ;; procedure, like sort), sliding window, convolve, rotate, apply

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
   (define second   fallback-second)
   (define third    fallback-third)
   (define fourth   fallback-fourth)
   (define fifth    fallback-fifth)
   (define sixth    fallback-sixth)
   (define seventh  fallback-seventh)
   (define eighth   fallback-eighth)
   (define ninth    fallback-ninth)
   (define tenth    fallback-tenth)
   (define last     fallback-last)
   (define for-each fallback-for-each)
   (define member?  fallback-member?)

   ;; Derived buildings, depend on either kind of basic building
   ;; Method names and generic function names are different, because they
   ;; have a different interface.
   (define range-method fallback-range)
   (define make-method  fallback-make)
   (define build-method fallback-build)

   ;; Derived transducers, need both a way to traverse and a way to build
   (define map            fallback-map)
   (define filter         fallback-filter)
   (define reverse        fallback-reverse)
   (define append         fallback-append)
   (define remove         fallback-remove)
   (define remove*        fallback-remove*)
   (define take           fallback-take)
   (define drop           fallback-drop)
   (define split-at       fallback-split-at)
   (define take-right     fallback-take-right)
   (define drop-right     fallback-drop-right)
   (define split-at-right fallback-split-at-right)
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
    (define second   l:second)
    (define third    l:third)
    (define fourth   l:fourth)
    (define fifth    l:fifth)
    (define sixth    l:sixth)
    (define seventh  l:seventh)
    (define eighth   l:eighth)
    (define ninth    l:ninth)
    (define tenth    l:tenth)
    (define last     l:last)
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
    (define range-method l:range)
    (define make-method  make-list)
    (define build-method build-list)
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
    (define take           l:take)
    (define drop           l:drop)
    (define split-at       l:split-at) 
    (define take-right     l:take-right)
    (define drop-right     l:drop-right)
    (define split-at-right l:split-at-right)
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
    (define (last v) (vector-ref v (sub1 (vector-length v))))
    ;; no structural building
    (struct vector-builder (l) #:mutable #:transparent
            #:methods gen:builder
            [(define (add-next x v)
               (set-vector-builder-l! v (r:cons x (vector-builder-l v))))
             (define (finalize v)
               (list->vector (r:reverse (vector-builder-l v))))])
    (define (make-builder _) (vector-builder '()))
    (define make-method   make-vector)
    (define build-method  build-vector)
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

