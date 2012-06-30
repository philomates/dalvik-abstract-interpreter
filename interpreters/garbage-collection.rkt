#lang racket

(require "structs.rkt")
(provide garbage-collect)

; State -> State
(define (garbage-collect st)
  (let*-values
    ([(fp) (state-frame-pointer st)]
     [(σ) (state-store st)]
     ; addresses in the current framepointer + object field addresses
     [(fp-addrs field-addrs)
      (for/fold
        ([fps '()] [fields '()])
        ([key (hash-keys σ)])
        (match key
               [`(,v ,(? (λ (x) (equal? fp x)))) (values (cons key fps) fields)]
               [`(,a ,b ...) (values fps fields)]
               [otherwise (values fps (cons key fields))]))]
     [(init-addrs) (cons (state-kaddr st) fp-addrs)]
     [(new-σ) (build-new-store (make-immutable-hash) σ init-addrs)]
     [(new-σ-with-fields)
      (foldl (λ (key h) (hash-set h key (hash-ref σ key))) new-σ field-addrs)])
    (struct-copy state st [store new-σ-with-fields])))

; Recursively find addresses reachable by kont
;
; Funk -> Hash -> (set addrs) -> [addrs]
(define (addrs-touched-by-kont k σ visited-kaddrs)
  (if (or (not (funk? k)) (equal? k 0))
    `()
    (let* ([fp (funk-frame-pointer k)]
           ; grab all addresses in store that use frame-pointer fp
           [fp-addrs
             (filter
               (λ (k)
                  (match k
                         [`(,v ,(? (λ (x) (equal? fp x)))) #t]
                         [otherwise #f]))
               (hash-keys σ))])

      (if (or (equal? (funk-kaddr k) 0)
              (equal? (funk-kaddr k) 'halt))
        ; stop recursion if we hit 0 or 'halt
        fp-addrs
        (let*
          ([parent-kaddrs (set->list (hash-ref σ (funk-kaddr k)))]
           [unvisited-parent-kaddrs
             (filter (λ (fnk) (not (set-member? visited-kaddrs (funk-kaddr fnk)))) parent-kaddrs)]
           [new-visited-kaddrs
             (set-add (set-union (set (map funk-kaddr parent-kaddrs)) visited-kaddrs) (funk-kaddr k))])
          (append
            fp-addrs
            (foldl
              append
              '()
              (map (λ (k) (addrs-touched-by-kont k σ new-visited-kaddrs)) unvisited-parent-kaddrs))))))))


; visit queue of addresses, pulling key/values out of old-σ and putting them in new-σ
; if addr points to kont, find all the addresses reachable by kont
;
; (hash addr set) -> (hash addr set) -> [addrs] -> (hash addr set)
(define (build-new-store new-σ old-σ queue)
  (if (null? queue)
    new-σ
    (if (hash-has-key? new-σ (car queue))
      ; skip already visited elements
      (build-new-store new-σ old-σ (cdr queue))
      ; grab store value of first elem in queue
      ; store that in new-σ & if there are kont values, add addrs they touch to queue
      (let* ([addr (car queue)]
             [store-value (hash-ref old-σ addr)]
             [konts (filter (λ (v) (funk? v)) (set->list store-value))]
             [updated-σ (hash-set new-σ addr store-value)])
        (if (not (null? konts))
          (let
            ([addrs-from-konts
               (foldl append '()
                      (map
                        (λ (k) (cons (funk-kaddr k) (addrs-touched-by-kont k old-σ (set))))
                        konts))])
            (build-new-store updated-σ old-σ (append addrs-from-konts (cdr queue))))
          (build-new-store updated-σ old-σ (cdr queue)))))))
