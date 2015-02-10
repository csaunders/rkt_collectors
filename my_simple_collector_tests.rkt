#lang plai/collector
(require "my_simple_collector.rkt")

;; Assertions / Helpers
(define (assert-fail success-message failure-message fn)
  (with-handlers ([exn:fail? (lambda (exn) success-message)])
    (fn)
    failure-message))

;; Finding Free Memory
(test ;; finding free memory
 (with-heap (vector 'x 'x 'x 'free 'x) (free-memory))
 '(3))

;; Finding Slots in Memory
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

;; Finding Available Addresses
(test ;; finding available addresses for cons cells
 (let ([v (make-vector 12 'free)])
   (with-heap v (find-space-for 'cons (lambda () '()))))
 '(0 1 2))

(test ;; finding available addresses for flat data
 (let ([v (make-vector 12 'free)])
   (with-heap v (find-space-for 'prim (lambda () '()))))
 '(0 1))

(test ;; finding available addresses for memory that is very fragmented
 (let ([v (vector 'x 'x 'free 'x 'free 'x 'x 'free 'free)])
   (with-heap v (find-space-for 'prim (lambda () '()))))
 '(7 8))

(test ;; finding available space when garbage has to be collected
 (let ([v (make-vector 12 'x)])
   (with-heap v
              (with-roots '()
                          (find-space-for 'prim (lambda () '())))))
 '(0 1))

(test ;; throws an error when there is not enough space to fit a item
 (assert-fail
  "error raised"
  "error not raised"
  (lambda ()
    (let ([v (make-vector 6 'free)])
      (with-heap v
                 (let
                     ([p1 (gc:alloc-flat 1)]
                      [p2 (gc:alloc-flat 2)])
                   (with-roots (list p1 p2)
                               (find-space-for 'cons '())))))))
 "error raised")

;; Sweeping
(test ;; that the list of addresses causes their heap locations to get set to 'free
 (let [(v (vector 0 1 2 3 4 5 6))]
   (with-heap v
            (sweep '(0 3 4 6))
            v))
 (vector 'free 1 2 'free 'free 5 'free))

;; Allocator Initialization
(test (let ([v (make-vector 12 'x)])
        (with-heap v (init-allocator))
        v)
      (make-vector 12 'free))

;; Cons Cell Allocation
(test ;;allocating a cons cell
 (let ([v (make-vector 4 'free)])
   (with-heap v
              (begin ;; Because we are modifying global variables we need to fix the heap ptr
               (gc:cons 8 2)))
   v)
 (vector 'cons 8 2 'free))

(test ;;allocating a cons cell returns the address of the 'cons
 (let ([v (make-vector 8 'free)])
   (with-heap v
              (begin
                (allocate-cons 8 2)
                (allocate-cons 8 3))))
 3)

;; Cons Cell Checking
(test
 (with-heap (vector 'free 'free 'prim 2 'cons 2 7 'prim 3)
            (gc:cons? 4))
 #t)

(test
 (with-heap (vector 'free 'free 'prim 2 'cons 2 7 'prim 3)
            (gc:cons? 2))
 #f)

;; Cons Cell Head
(test ;; gc:first for a heap ref to a actual cons-cell
 (with-heap (vector 'prim 1 'prim 2 'cons 0 2)
            (gc:first 4))
 0)

(test ;; gc:first for a heap-ref that is not a cons cell
 (assert-fail
  "error raised"
  "error not raised"
  (lambda ()
    (with-heap (vector 'prim 1 'cons 0 1)
              (gc:first 0))))
 "error raised")

;; Cons Cell Tail
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

;; Heap Marking

(test ;; that calling reachable-from-root returns true if the address can be found in the set
 (with-heap
  (vector 'prim 1 'prim 2 'cons 0 2 'prim 3 'free)
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