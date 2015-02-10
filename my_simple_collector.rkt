#lang plai/collector
(provide
 free-memory
 slot-of-size
 find-space-for
 allocate-cons
 mark
 perform-mark
 sweep)

(define (free-memory)
  (filter-map (lambda (addr)
                (and (eq? 'free (heap-ref addr)) addr))
              (build-list (heap-size) values)))

(define (oom! err)
  (error err "out of memory"))

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
           (let ([candidate-rest
                  (slot-of-size (- size 1) (cdr memory-space))])
             (if (and (= (- size 1) (length candidate-rest)) (or (empty? candidate-rest)(= (+ 1 prospect) (car candidate-rest))))
                 (append (list prospect) candidate-rest)
                 (slot-of-size size (cdr memory-space))))]
          [else (slot-of-size size (cdr memory-space))]))))

(define (find-space-for type get-roots)
  (let* ([address-finder (lambda ()
                           (cond
                             [(eq? 'prim type) (slot-of-size 2 (free-memory))]
                             [(eq? 'cons type) (slot-of-size 3 (free-memory))]))]
         [available-addresses (address-finder)])
    (if (empty? available-addresses)
        (begin
          (sweep (mark get-roots))
          (let ([new-available-addresses (address-finder)])
            (if (empty? new-available-addresses)
                (oom! 'find-space-for)
                new-available-addresses)))
        available-addresses)))


(define (memory-addresses item)
  (let ((value (if (number? item)(heap-ref item) #f)))
    (cond
      [(eq? 'prim value) (list item (+ 1 item))]
      [(eq? 'cons value) (let ([first (+ 1 item)]
                             [rest  (+ 2 item)])
                         (list item first rest (heap-ref first) (heap-ref rest)))]
      [else '(item)])))

(define (sweep addresses)
  
  (unless (empty? addresses)
    (begin
      (heap-set! (car addresses) 'free)
      (sweep (cdr addresses))
      addresses)))

(define (init-allocator)
  (let ([memory (build-list (heap-size) values)])
    (begin
      (sweep memory))))

;; Flat Value Allocation

(define (allocate-flat ptr value)
  (begin
    (heap-set! ptr 'prim)
    (heap-set! (+ 1 ptr) value)))
 
(define (gc:alloc-flat value)
  (let [(ptr (car (find-space-for
              'prim
              (lambda ()
                (if (procedure? value)
                    (append (procedure-roots value)
                            (get-root-set))
                    (get-root-set))))))]
  (allocate-flat ptr value)
  ptr))

;; Cons Cell Allocation

(define (allocate-cons first rest)
  (let [(ptr (car (find-space-for 'cons (lambda () (get-root-set)))))]
    (begin
      (heap-set! ptr 'cons)
      (heap-set! (+ ptr 1) first)
      (heap-set! (+ ptr 2) rest))
    ptr))

(define (gc:cons f r)
  (allocate-cons f r))
 
(define (gc:cons? a)
 (eq? (heap-ref a) 'cons))

(define (raise-if-not-cons a)
  (when (not (gc:cons? a))
    (error 'gc:first "expects address of cons")))
 
(define (gc:first a)
  (raise-if-not-cons a)
  (heap-ref (+ 1 a)))
 
(define (gc:rest a)
  (raise-if-not-cons a)
  (heap-ref (+ 2 a)))
  
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
(define (mark . get-roots)
  (let* ([roots (if (not (eq? '() get-roots)) ((car get-roots)) (get-root-set))]
         [greys (map
                 (lambda (root)
                   ;; Hack -- How do I create roots when testing?
                   (if (root? root)
                       (read-root root)
                       root))
                 roots)]
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