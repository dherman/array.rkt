#lang racket/base

(require racket/match)

(provide (rename-out [array* array])
         build-array
         array-ref array-set
         array-for-each
         array-length array->list)

;; TODO: array-map, array-foldl, array-foldr
;; TODO: array, list->array

;; (data a)
(define-struct array ([data #:mutable]))

;; exact-nonnegative-integer * a * (data a)
(define-struct diff (index value data))

;; (data a) ::= (vector a) | (diff a)

;; exact-nonnegative-integer * a -> (array a)
(define array*
  (procedure-rename
   (lambda (n [x #f])
     (make-array (make-vector n x)))
   'array))

;; exact-nonnegative-integer * (exact-nonnegative-integer -> a) -> (array a)
(define (build-array n proc)
  (make-array (build-vector n proc)))

;; (array a) -> void
(define (reroot! t)
  (match (array-data t)
    [(? vector?) (void)]
    [(struct diff (i v t*))
     (reroot! t*)
     (let* ([n (array-data t*)]
            [v* (vector-ref n i)])
       (vector-set! n i v)
       (set-array-data! t n)
       (set-array-data! t* (make-diff i v* t)))]))

;; (array a) * exact-nonnegative-integer -> a
(define (array-ref t i)
  (let ([a (array-data t)])
    (if (vector? a)
        (vector-ref a i)
        (begin
          (reroot! t)
          (vector-ref (array-data t) i)))))

;; (array a) * exact-nonnegative-integer * a -> (array a)
(define (array-set t i v)
  (reroot! t)
  (let* ([n (array-data t)]
         [old (vector-ref n i)])
    (if (equal? old v)
        t
        (begin
          (vector-set! n i v)
          (let ([res (make-array n)])
            (set-array-data! t (make-diff i old res))
            res)))))

(define (impure f t)
  (reroot! t)
  (f (array-data t)))

(define (apply-impure f ts)
  (for-each reroot! ts)
  (apply f ts))

(define (array-length t)
  (impure vector-length t))

(define (array->list t)
  (impure vector->list t))

(define (array-for-each f t)
  (impure (lambda (d) (vector-for-each d f)) t))
