;; http://en.wikipedia.org/wiki/Newton%27s_method
#lang plai/mutator ;; Tell Racket to use the 'mutator' language
(allocator-setup "my_simple_collector.rkt" 64) ;; Set up an allocator with 128 spaces

;; We aren't going to figure out what the derivative is, we will simply
;; provide the derivative ourselves
(define (newtons-method-iter guess number fn derivfn iterations)
  (if (= 0 iterations)
      guess
      (newtons-method-iter
       (- guess (/ (- (fn guess) number)
                   (derivfn guess)))
       number
       fn
       derivfn
       (- iterations 1))))

(define (square x) (* x x))
(define (double x) (* 2 x))

;; The square root of 612 is ~24.738633754
;;(newtons-method-iter 10 612 square double 2)
(newtons-method-iter 10 612 square double 5)
