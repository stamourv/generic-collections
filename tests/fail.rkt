#lang racket

(require "../main.rkt")

(struct not-really-a-collection ()
        #:methods gen:collection
        [])

(module+ test
  (require rackunit)
  (check-exn exn:fail? (lambda () (length 'not-a-collection)))
  (check-exn exn:fail? (lambda () (length (not-really-a-collection)))))
