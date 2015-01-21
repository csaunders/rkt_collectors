#lang plai/collector
(define heap-ptr 'uninitialized-heap-ptr)
(define free-memory '())

(define (set-heap-ptr! pos)
  (if (eq? 'free (heap-ref pos))
      (set! heap-ptr pos)
      (set-heap-ptr! (+ 1 pos))))

(test ;; That set-heap-ptr sets the pointer to the position of the first free entry
 (with-heap (vector 1 2 3 'free 'free 'free) (set-heap-ptr! 0)
            heap-ptr)
 3)

(define (oom? offset)
  (> (+ heap-ptr offset) (heap-size)))

(define (oom! err)
  (error err "out of memory"))

(define (incr-heap! . amount)
  (set! heap-ptr
        (if (not (eq? '() amount))
            (+ (car amount) heap-ptr)
            (+ 1 heap-ptr)))
  heap-ptr)

(define (slot-of-size size memory-space)
  (if (empty? memory-space)
      '()
      (let* ([prospect (car memory-space)]
             [has-more (not (empty? (cdr memory-space)))]
             [neighbour-is-bigger-by-one
              (if has-more
                  (= (+ 1 prospect) (cadr memory-space))
                  #f)])
        (cond
          [(= 1 size) (list prospect)]
          [(and has-more (not neighbour-is-bigger-by-one)) (slot-of-size size (cdr memory-space))]
          [(and has-more neighbour-is-bigger-by-one)
           (let* ([candidate-rest (slot-of-size (- size 1) (cdr memory-space))])
             (if (and (= (- size 1) (length candidate-rest)) (or (empty? candidate-rest)(= (+ 1 prospect) (car candidate-rest))))
                 (append (list prospect) candidate-rest)
                 (slot-of-size size (cdr memory-space))))]
          [else (slot-of-size size (cdr memory-space))]))))

(test ;; slot-of-size when there is free space but not consecutively
 (let ([memory '(0 2 4 6 8)])
   (slot-of-size 2 memory))
 empty)

(test ;; slot-of-size when there is not just enough free consecutive space
 (let ([memory '(0 1 2 4 5)])
   (slot-of-size 4 memory))
 empty)

(test ;; slot-of-size when there is enough free consecutive space
 (let ([memory '(0 1 2 3 4 5)])
   (slot-of-size 4 memory))
 '(0 1 2 3))

(test ;; slot-of-size when the free memory is not right at the start
 (let ([memory '(0 1 3 4 6 7 8 9)])
   (slot-of-size 4 memory))
 '(6 7 8 9))

(define (find-space-for type)
  (let ([available-address  (cond
                              [(eq? 'prim type) (slot-of-size 2 free-memory)]
                              [(eq? 'cons type) (slot-of-size 3 free-memory)])])
    (if (not available-address)
        (oom! 'find-space-for)
        available-address)))

(define (memory-addresses item)
  (cond
    [(gc:flat? item) (list item (+ 1 item))]
    [(gc:cons? item) (let ([first (+ 1 item)]
                           [rest  (+ 2 item)])
                       (list item first rest (heap-ref first) (heap-ref rest)))]
    [else '(item)]))

(define (sweep addresses)
  (unless (empty? addresses)
    (begin
      (heap-set! (car addresses) 'free)
      (sweep (cdr addresses))
      addresses)))

(test
 (let [(v (vector 0 1 2 3 4 5 6))]
   (with-heap v
            (sweep '(0 3 4 6))
            v))
 (vector 'free 1 2 'free 'free 5 'free))

;; Allocator Initialization

(define (init-allocator)
  (let ([memory (build-list (heap-size) values)])
    (begin
      (sweep memory)
      (set! heap-ptr 0)
      (set! free-memory memory))))

(test (let ([v (make-vector 12 'x)])
        (with-heap v (init-allocator))
        v)
      (make-vector 12 'free))

;; Flat Value Allocation

(define (allocate-flat p)
  (if (oom? 2)
      (oom! 'gc:alloc-flat)
      (begin
        (heap-set! heap-ptr 'prim)
        (heap-set! (+ 1 heap-ptr) p)
        (set! heap-ptr (+ 2 heap-ptr))
        
        (- heap-ptr 2))))
 
(define (gc:alloc-flat p)
  (when (oom? 2)
    (sweep))
  (allocate-flat p))

;; Cons Cell Allocation

(define (allocate-cons first rest)
  (let ([cons-ptr heap-ptr])
    (begin
      (when (oom? 3)
        (oom! 'gc:cons))
      (heap-set! heap-ptr 'cons)
      (heap-set! (incr-heap!) first)
      (heap-set! (incr-heap!) rest)
      (incr-heap!)
      )
    cons-ptr))

(test ;;allocating a cons cell
 (let ([v (make-vector 4 'free)])
   (with-heap v
              (begin ;; Because we are modifying global variables we need to fix the heap ptr
               (init-allocator)
               (allocate-cons 8 2)))
   v)
 (vector 'cons 8 2 'free))

(test ;;allocating a cons cell correctly increments the heap-ptr
 (let ([v (make-vector 4 'free)])
   (with-heap v
              (begin ;; Because we are modifying global variables we need to fix the heap ptr
                (init-allocator)
                (allocate-cons 8 2)))
   heap-ptr)
 3)

(test ;;allocating a cons cell returns the address of the 'cons
 (let ([v (make-vector 8 'free)])
   (with-heap v
              (begin
                (init-allocator)
                (allocate-cons 8 2)
                (allocate-cons 8 3))))
 3)

(define (gc:cons f r)
  (when (oom? 3)
    (sweep))
  (allocate-cons f r))
 
(define (gc:cons? a)
 (eq? (heap-ref a) 'cons))

(test
 (with-heap (vector 'free 'free 'prim 2 'cons 2 7 'prim 3)
            (gc:cons? 4))
 #t)

(test
 (with-heap (vector 'free 'free 'prim 2 'cons 2 7 'prim 3)
            (gc:cons? 2))
 #f)

(define (raise-if-not-cons a)
  (when (not (gc:cons? a))
    (error 'gc:first "expects address of cons")))
 
(define (gc:first a)
  (raise-if-not-cons a)
  (heap-ref (+ 1 a)))

(test ;; gc:first for a heap ref to a actual cons-cell
 (with-heap (vector 'prim 1 'prim 2 'cons 0 2)
            (gc:first 4))
 0)

(define (assert-fail success-message failure-message fn)
  (with-handlers ([exn:fail? (lambda (exn) success-message)])
    (fn)
    failure-message))

(test ;; gc:first for a heap-ref that is not a cons cell
 (assert-fail
  "error raised"
  "error not raised"
  (lambda ()
    (with-heap (vector 'prim 1 'cons 0 1)
              (gc:first 0))))
 "error raised")
 
(define (gc:rest a)
  (raise-if-not-cons a)
  (heap-ref (+ 2 a)))

(test ;; gc:rest for a heap-ref to an actual cons-cell
 (with-heap
  (vector 'prim 1 'prim 2 'cons 0 2)
  (gc:rest 4))
 2)

(test ;; gc:rest for a heap-ref to an invalid cons-cell
 (assert-fail
  "error raised"
  "error not raised"
  (lambda ()
    (with-heap (vector 'prim 1 'cons 0 1)
               (gc:rest 0))))
 "error raised")
  
(define (gc:set-first! a f)
  (raise-if-not-cons a)
  (heap-set! (+ 1 a) f))

(define (gc:set-rest! a r)
  (raise-if-not-cons a)
  (heap-set! (+ 2 a) r))
 
(define (gc:flat? a)
 (eq? (heap-ref a) 'prim))
 
(define (gc:deref a)
 (heap-ref (+ 1 a)))
 
;; Actual Garbage Collection Business
(define (mark)
  (let* ([greys (get-root-set)]
         [whites (remv* greys (build-list (heap-size) values))])
    (perform-mark '() greys whites)))

(define (perform-mark blacks greys whites)
  (if (empty? greys)
      (remv* blacks whites)
      (let* ([item (car greys)]
             [addresses (memory-addresses item)]
             [new-blacks (append blacks (list item))]
             [new-greys (append (cdr greys) (cdr addresses))])
        (perform-mark new-blacks new-greys whites))))

(test ;; that calling reachable-from-root returns true if the address can be found in the set
 (with-heap (vector 'prim 1 'prim 2 'cons 0 2 'prim 3 'free)
            (with-roots '(4) (mark)))
 '(7 8 9))
  
(test ;; that calling mark when there are no roots returns all the memory addresses
 (with-heap (make-vector 4)
            (with-roots '()
                        (mark)))
 (list 0 1 2 3))

(test ;; that calling mark when there are live children does not include those addresses
 (with-heap
        ;chld   ;chld   ;root     ;waste
  (vector 'prim 1 'prim 2 'cons 0 2 'prim 4)
  (with-roots '(4) (mark)))
 (list 7 8))

(test ;; that calling mark when there is a longer collection of cons cells
 (with-heap
  (vector 'cons 9 3 'cons 11 6 'cons 13 15 'prim 1 'prim 2 'prim 3 'prim empty)
  (with-roots '(0) (mark)))
 '())

(test ;; that calling incr-heap! with no arguments increases the heap by 1
 (begin
   (set! heap-ptr 0)
   (incr-heap!))
 1)

(test ;; that calling incr-heap! with a single argument increments the heap by that amount
 (begin
   (set! heap-ptr 0)
   (incr-heap! 5))
 5)

(test ;; that calling incr-heap! with a non-zero starting value returns heap end location
 (begin
   (set! heap-ptr 4)
   (incr-heap! 2))
 6)

(test ;; that calling incr-heap! with multiple arguments only uses the first
 (begin 
   (set! heap-ptr 1)
   (incr-heap! 4 5 6 7))
 5)
