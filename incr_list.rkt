#lang plai/mutator ;; Tell Racket to use the 'mutator' language
(allocator-setup "simple_collector.rkt" 64) ;; Set up an allocator with 128 spaces

(define (incr-list lst)
  (if (empty? lst)
      lst
      (cons
        (+ 1 (first lst))
        (incr-list (rest lst)))))

(incr-list '(1 2 3 4 5))
