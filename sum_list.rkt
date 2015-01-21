#lang plai/mutator ;; Tell Racket to use the 'mutator' language
(allocator-setup "simple_collector.rkt" 64) ;; Set up an allocator with 128 spaces

(define (sum-list lst)
  (if (empty? lst)
      0
      (+ (first lst) (sum-list (rest lst)))))

(sum-list '(1 2 3))