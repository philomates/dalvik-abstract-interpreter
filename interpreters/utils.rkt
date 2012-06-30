#lang racket

(require "structs.rkt")

(provide (all-defined-out))

(define (join-symbols l r)
  (string->symbol
    (string-append
      (symbol->string l) "/"
      (symbol->string r))))


; decimal/binary funcs to handle overflow type things in the dalvik byte code.

(define (decimal->binary dec)
  (letrec
    ([inner
       (λ (dec)
          (let* ([m (modulo dec 2)]
                 [q (floor (/ dec 2))])
            (if (eq? q 0)
              (cons m '())
              (cons m (inner q)))))])
    (reverse (inner dec))))

; assume no leading 0's
; doesn't trim binary numbers longer than bit-size
(define (binary->decimal bin
                         [twos-complement? #t]
                         [bit-size 0])
  (letrec
    ; standard binary to decimal recursive func
    ([normal-bin->dec
       (λ (b)
          (if (null? b)
            0
            (+ (* (expt 2 (sub1 (length b))) (car b))
               (normal-bin->dec (cdr b)))))])

  ; handle twos complement
  (if twos-complement?
    (let*
      ([bits (max bit-size (length bin))]
       [bias (expt 2 (- bits 1))])
      (if (eq? (length bin) bit-size)
          ; negative number
          (- (binary->decimal (cdr bin) #f) bias)
          ; positive number
          (binary->decimal bin #f)))
    (normal-bin->dec bin))))

; -------------------------------------
; Debugging utils
; -------------------------------------
; print out keys that have different values in the new store
(define (diff-stores old-state new-state [fst-state #f])
  (begin
    (if fst-state
      (displayln "Unique to 1st Store\n-------------")
      (displayln "Unique to 2nd Store\n-------------"))
    (let* ([new-store (state-store new-state)]
           [old-store (state-store old-state)]
           [new-keys (hash-keys new-store)])
      (for-each
        (λ (key)
           (let ([value
                   (if (hash-has-key? old-store key)
                     (set-subtract (hash-ref new-store key) (hash-ref old-store key))
                     (format "NEW ~a" (hash-ref new-store key)))])
             (when (or (not (set? value)) (not (set-empty? value)))
               (begin (displayln (format "key: ~a" key))
                      (pretty-print value)))))
        new-keys))
    (when fst-state (diff-stores new-state old-state))))

; count number of elements in each key as a way of metric-izing stores
(define (store-metric σ)
  (for/fold
    ([sum 0])
    ([value (hash-values σ)])
    (+ sum (length (set->list value)))))

; sort stores by total elements
(define (sort-stores stores)
  (sort stores (λ (x y) (<= (store-metric x) (store-metric y)))))

; find all states that have the same statements as a given state
(define (find-similar-states root states)
  (filter (λ (s) (equal? (state-statements root) (state-statements s))) states))

#|
(decimal->binary 95)
(decimal->binary 255)
(binary->decimal (decimal->binary 4) #f 4)
|#
