#lang plai/collector

(define (required-bytes-for n)
  (ceiling (/ n 8)))

(test (required-bytes-for 16) 2)
(test (required-bytes-for 7) 1)
(test (required-bytes-for 24) 3)

(define heap-bytemask-size 'unitialized-bytemask-size)
(define heap-pointer 'unitialized-heap-pointer)

(define (setup testfn)
  (begin
    (set-heap-pointer)
    (set-bytemask-size 0)
    (testfn)))

(define (set-heap-pointer)
  (set! heap-pointer 0))

(define (set-bytemask-size size)
  (set! heap-bytemask-size size))

(define (heapable-byte bstr)
  (let
    ([data
      (if (= 2 (bytes-length bstr))
          bstr
          (bytes-append bstr #"\0"))])
    (integer-bytes->integer data #f)))

(test (heapable-byte #"\5") 5)
(test (heapable-byte #"\377\0") 255)

(define (set-bytemask-bytes num-bytes)
  (if
    (= 0 num-bytes)
    heap-pointer
    (begin
      (heap-set! heap-pointer (heapable-byte #"\0"))
      (set! heap-pointer (+ 1 heap-pointer))
      (set-bytemask-bytes (- num-bytes 1))
      )
    )
  )

(define (bytemask-byte int)
  (subbytes
    (integer->integer-bytes int 2 #f)
    0 1))

(test (bytemask-byte 1) #"\1")

(define (get-bytemask)
  (letrec ([collect-bytes
           (lambda (n)
            (if (= (+ 1 n) heap-bytemask-size)
                (bytemask-byte (heap-ref n))
                (bytes-append (bytemask-byte (heap-ref n)) (collect-bytes (+ 1 n))))
            )])
    (collect-bytes 0)))

(define (set-bytemask bytemask position)
  (if (= position heap-bytemask-size)
      (void)
      (heap-set! position )))

(define (mark-position bytemask position) (void))



(define (init-allocator)
  (begin
    (set-heap-pointer)
    (set-bytemask-size
      (set-bytemask-bytes (required-bytes-for (heap-size))))
    ))

(test
  (with-heap
    (make-vector 16)
    (begin
      (init-allocator)
      heap-bytemask-size))
  2)

(test
  (with-heap
    (make-vector 15)
    (begin
      (init-allocator)
      heap-bytemask-size))
  2)

(test
  (with-heap
    (make-vector 17)
    (begin
      (init-allocator)
      heap-bytemask-size))
  3)

(test
  (with-heap
    (make-vector 8)
    (begin (init-allocator) (heap-ref 0)))
  1)

(define (gc:alloc-flat p) 0)

(define (gc:cons f r) 0)

(define (gc:cons? a) #f)

(define (gc:first a) 0)

(define (gc:set-first! a f) (void))

(define (gc:rest a) 0)

(define (gc:set-rest! a r) (void))

(define (gc:flat? a) #f)

(define (gc:deref a) 'notImplemented)
