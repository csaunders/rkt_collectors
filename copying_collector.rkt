#lang plai/collector
;; Testing Utilities
(define (test-raises fn)
  (test
   (with-handlers
       ([exn:fail? (lambda (exn) "exception raised")])
     (fn))
    "exception raised"))

(define current-space #f)

(define (space-size)
  (/ (heap-size) 2))

(define (init-allocator)
  (begin
    (set! current-space 0)))

(define (next-space space)
  (modulo
   (+ space (space-size))
   (heap-size)))

(define (change-space space)
  (set! current-space (next-space space)))

(define (memory-spaces)
  (list
   (list 'old-space (list 'start current-space 'size (space-size)))
   (list 'new-space (list 'start (next-space current-space) 'size (space-size)))))

(test ;; initializing the allocator splits memory into two distinct parts
 (with-heap (make-vector 12 'x)
            (begin
              (init-allocator)
              (memory-spaces)))
 '((old-space (start 0 size 6))
   (new-space (start 6 size 6))))
 

(define (gc:deref addr)
  (if (and (> addr current-space)
           (< addr (+ current-space (space-size))))
      (heap-ref addr)
      (error 'gc:deref "Invalid memory Address")))

(test ;; when dereferencing valid memory
 (with-heap (vector 1 2 3 4 5 6 7 8 9 10)
              (gc:deref 2))
 3)

(test-raises ;; when dereferencing an invalid memory address
 (lambda ()
   (with-heap (make-vector 12 'x)
              (gc:deref 6))))

(define (gc:alloc-flat value)
  (let* [(rootfn (lambda ()
                  (if (procedure? value)
                      (append (procedure-roots value)
                              (get-root-set))
                      (get-root-set))))
         (ptr (find-memory 2 rootnf))]
    (begin
      (heap-set! ptr 'flat)
      (heap-set! (+ ptr 1) value))))

(define (gc:cons first rest) '())

(define (gc:first addr) '())

(define (gc:rest addr) '())

(define (gc:set-first! addr first) '())

(define (gc:set-rest! addr rest) '())

(define (gc:cons? addr) '())

(define (gc:flat? addr) '())