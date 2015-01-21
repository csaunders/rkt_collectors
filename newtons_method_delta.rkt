;; http://en.wikipedia.org/wiki/Newton%27s_method
#lang plai/mutator ;; Tell Racket to use the 'mutator' language
(allocator-setup "simple_collector.rkt" 64) ;; Set up an allocator with 128 spaces

(define (newtons-method-delta number fn derivfn delta))
