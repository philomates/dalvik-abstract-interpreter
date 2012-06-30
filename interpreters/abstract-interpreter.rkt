#lang racket

; Work in Progress Abstract interpreter for the Dalvik VM
; that uses abstract numbers and can create a graph

; meta-function lookup functions
(require "meta-functions.rkt")
; build class, static method, & label tables
(require "meta-function-builder.rkt")
; low level numeric functions
(require "utils.rkt")
; functions to help classify instructions
(require "dalvik-utils.rkt")
; functions to help build *.dot graphs
(require "graph-utils.rkt")

(require "garbage-collection.rkt")

(require "structs.rkt")

(define k 1)
(define dummy-string (object '() 'java/lang/String))

; -------------------------------------
; Allocation
; -------------------------------------
(define (alloc time) time)

(define (tick time call-site) (take (cons call-site time) k))

; -------------------------------------
; Transition Function
; -------------------------------------
(define (gc-next metas current-state)
  (map garbage-collect (next metas current-state)))

(define (next metas current-state)
  (if (state? current-state)
      (let* ([current-statements (state-statements current-state)]
             [fp (state-frame-pointer current-state)]
             [σ (state-store current-state)]
             [kaddr (state-kaddr current-state)]
             [k (set->list (hash-ref σ kaddr))]
             [t (state-time current-state)]
             [v-exception 'bunk]
             [s (rest current-statements)]
             [this-statement (first current-statements)]
             ; NOTE: time ticks on entry to new call-sites
             [t-prime (tick t this-statement)])
        (match this-statement

          ; Move
          ; -----------------
          [`(move ,rd ,rs)
           (let ([σ-prime (union-into-store σ `(,rd ,fp) (lookup/fp σ fp rs))])
             (list (state s fp σ-prime kaddr t-prime)))]

          [`(move-exception ,r)
           (let ([σ-prime (union-into-store σ `(,r ,fp) (lookup/fp σ fp v-exception))])
             (list (state s fp σ-prime kaddr t-prime)))]

          [`(move-result ,r)
           (let ([σ-prime (union-into-store σ `(,r ,fp) (lookup/fp σ fp 'ret))])
             (list (state s fp σ-prime kaddr t-prime)))]

          [`(move-object ,rd ,rs)
           (let ([σ-prime (union-into-store σ `(,rd ,fp) (lookup/fp σ fp rs))])
             (list (state s fp σ-prime kaddr t-prime)))]

          [`(move-result-object ,r)
           (let ([σ-prime (union-into-store σ `(,r ,fp) (lookup/fp σ fp 'ret))])
             (list (state s fp σ-prime kaddr t-prime)))]

          ; Return
          ; -----------------
          ['(return-void)
           (if (equal? k '(halt))
             '()
             (map
               (λ (kont)
                  (match kont
                         [(funk s-prime fp-prime kaddr-prime)
                          (state s-prime fp-prime σ kaddr-prime t-prime)]))
               k))]

          [`(return ,r)
           (if (equal? k '(halt))
             '()
             (map
               (λ (kont)
                  (match kont
                         [(funk s-prime fp-prime kaddr-prime)
                          (let ([σ-prime (union-into-store σ `(ret ,fp-prime) (lookup/fp σ fp r))])
                            (state s-prime fp-prime σ-prime kaddr-prime t-prime))]))
               k))]

          [`(return-object ,r)
           (if (equal? k '(halt))
             '()
             (map
               (λ (kont)
                  (match kont
                         [(funk s-prime fp-prime kaddr-prime)
                          (let ([σ-prime (union-into-store σ `(ret ,fp-prime) (lookup/fp σ fp r))])
                            (state s-prime fp-prime σ-prime kaddr-prime t-prime))]))
               k))]

          ; Const
          ; -----------------
          [`(const-string ,vx ,const)
           (let* ([obj (object-lookup metas 'java/lang/String)]
                  [σ-with-inits (object-init metas 'java/lang/String obj σ)]
                  [σ-prime (union-into-store σ-with-inits `(,vx ,fp) obj)])
             (list (state s fp σ-prime kaddr t-prime)))]

          [`(,(? (opcode-predicate "const")) ,vx ,const)
           (let ([σ-prime (union-into-store σ `(,vx ,fp) 'number)])
             (list (state s fp σ-prime kaddr t-prime)))]

          ; Switch
          ; -----------------
          ; TODO:
          ; sparse-switch packed-switch

          [`(goto ,l)
            (list (state (label-lookup metas l) fp σ kaddr t-prime))]

          ; Compare
          ; -----------------
          ; cmpl-float cmpg-float cmpl-double cmpg-double cmp-long
          [`(,(? (opcode-predicate "cmp")) ,rd ,r0 ,r1)
            (let* ([xs (map D (lookup/fp σ fp r0))]
                   [ys (map D (lookup/fp σ fp r1))]
                   [results
                     (for*/list ([x xs] [y ys])
                                (cmp-dispatch (car this-statement) x y))])
              (map (λ (val) (state s fp (union-into-store σ `(,rd ,fp) val) kaddr t-prime)) results))]



          ; Branches
          ; -----------------
          ; if-eq if-ne if-lt if-ge if-gt if-le
          [`(,(? (opcode-predicate "if-")) ,r0 ,r1 ,l)
            (list
              ; branch
              (state (label-lookup metas l) fp σ kaddr t-prime)
              ; don't branch
              (state s fp σ kaddr t-prime))]

          ; if-eqz if-nez if-ltz if-gez if-gtz if-lez
          [`(,(? (opcode-predicate "if-")) ,r ,l)
            (list
              ; branch
              (state (label-lookup metas l) fp σ kaddr t-prime)
              ; don't branch
              (state s fp σ kaddr t-prime))]

          ; Invokes
          ; -----------------

          ; Same as invoke-virtual but throw error instead of climbing inheritance ladder
          ; if virtual-lookup doesn't find the method.
          [`(invoke-interface ,rlst ,id ,types ...)
            (let* ([objs (filter (λ (o) (object? o)) (lookup/fp σ fp (car rlst)))]
                   [objs-w-interface
                     (filter
                       (λ (o)
                          (let ([attrs (class-attrs (class-lookup metas (object-class-path o)))])
                            (and
                              ; XXX: eventually attrs wont just be null or 'implements'!!!
                              (not (null? attrs))
                                (equal?
                                  (string-join (drop-right (regexp-split #px"\\/" (symbol->string id)) 1) "/")
                                  (symbol->string (last (car attrs)))))))
                       objs)]
                   [kaddr-prime (cons 'kont-addr t-prime)]
                   [σ-prime (union-into-store σ kaddr-prime (funk s fp kaddr))])

              ; for each possible object, create a new state that invokes that object's method
              (define states
                (filter
                 (λ (st) (state? st))
                 (map
                  (λ (obj)
                     (let ([implemented-id (string->symbol (string-append (symbol->string (object-class-path obj)) "/" (last (regexp-split #px"\\/" (symbol->string id)))))])
                       (invoke-method metas fp σ-prime kaddr-prime t-prime #t rlst implemented-id types obj)))
                  objs-w-interface)))

              (if (zero? (length states))
                  (error "method not found" this-statement)
                  states))]
          [`(invoke-direct ,rlst ,id ,types ...)
            (let* ([objs (filter (λ (o) (object? o)) (lookup/fp σ fp (car rlst)))]
                   [kaddr-prime (cons 'kont-addr t-prime)]
                   [σ-prime (union-into-store σ kaddr-prime (funk s fp kaddr))])

              ; for each possible object, create a new state that invokes that object's method
              (define states
                (filter
                 (λ (st) (state? st))
                 (map
                  (λ (obj) (invoke-method metas fp σ-prime kaddr-prime t-prime #t rlst id types obj))
                  objs)))

              (if (zero? (length states))
                  (error "method not found" this-statement)
                  states))]

          [(or `(invoke-virtual ,rlst ,id ,types ...)
               `(invoke-super ,rlst ,id ,types ...))
           ; XXX: what does one do when a method arg has multiple types in the store
           ;      e.g. (v1 stmt1) -> ('number <#object> 'number)
           ;      And also object that don't have the specified method?
           ; For now I will filter out non-objects...
            (let* ([objs (filter (λ (o) (object? o)) (lookup/fp σ fp (car rlst)))]
                   [kaddr-prime (cons 'kont-addr t-prime)]
                   [σ-prime (union-into-store σ kaddr-prime (funk s fp kaddr))])
              ; throw out any failed virtual lookups, leaving only states that successfully resolved
              (define states
                (filter
                 (λ (st) (state? st))
                 (map
                  (λ (obj) (invoke-method metas fp σ-prime kaddr-prime t-prime #f rlst id types obj))
                  objs)))

              (if (zero? (length states))
                  (error "method not found" this-statement)
                  states))]

          [`(invoke-static ,rlst ,id ,type ...)
            (let* ([kaddr-prime (cons 'kont-addr t-prime)]
                   [σ-prime (union-into-store σ kaddr-prime (funk s fp kaddr))]
                   [fp-prime (alloc t-prime)]
                   [static-method (method-lookup metas id)]
                   ; compute argument parameter mapping:
                   ;   parameter arguments are shift to the end of the
                   ;   registers available in the method being invoked.
                   [register-limit (method-reg-limit static-method)]
                   [start-register (- register-limit (length rlst))]
                   [rlst-mapping
                     (for/list ([r rlst]
                                [n (build-list (length rlst) (λ (x) (+ start-register x)))])
                               `((,(print-register n) ,fp-prime) ,(lookup/fp σ-prime fp r)))]
                   [σ-pp (foldl (λ (kv h) (union-into-store h (first kv) (second kv))) σ-prime rlst-mapping)]
                   [s-prime (method-statements static-method)])
              (begin
                (list (state s-prime fp-prime σ-pp kaddr-prime t-prime))))]

          ; TODO:
          ; invoke-virtual/range invoke-super/range invoke-direct/range
          ; invoke-static/range invoke-interface/range

          [`(new-instance ,r ,type)
           (let* ([obj (object-lookup metas type)]
                  [σ-with-inits (object-init metas type obj σ)]
                  [σ-prime (union-into-store σ-with-inits `(,r ,fp) obj)])
             (list (state s fp σ-prime kaddr t-prime)))]

          ; gets/puts
          ; -----------------
          ; instance get
          [`(,(? (opcode-predicate "iget")) ,rd ,rs ,id ,type)
            (match-let*
              ([objs (filter object? (lookup/fp σ fp rs))]
               ; grab field index from field-table in class-table
               [(list classPath fieldName) (regexp-split #px"\\." (symbol->string id))]
               [ft (class-field-table (class-lookup metas (string->symbol classPath)))]
               [f (hash-ref ft (string->symbol fieldName))]
               [index (field-index f)]
               ; Meow, for sanity preservation also grab type info
               [fieldType (field-type f)]
               ; lookup field symbol from object field list
               ; XXX: remove any objects that don't have enough field entries
               [fieldAddrs
                 (filter identity
                   (map
                     (λ (obj)
                        (if (< index (length (object-field-symbols obj)))
                          (list-ref (object-field-symbols obj) index)
                          #f))
                     objs))]
               [fieldVars (flatten (map (λ (fieldAddr) (set->list (hash-ref σ fieldAddr))) fieldAddrs))])
              (begin
                (unless (equal? fieldType type) (error "types don't match in iget"))
                (list (state s fp (union-into-store σ `(,rd ,fp) fieldVars) kaddr t-prime))))]

          ; instance put
          [`(,(? (opcode-predicate "iput")) ,rv ,rs ,id ,type)
            (match-let*
              ([objs (filter object? (lookup/fp σ fp rs))]
               ; grab field index from field-table in class-table
               [(list classPath fieldName) (regexp-split #px"\\." (symbol->string id))]
               [ft (class-field-table (class-lookup metas (string->symbol classPath)))]
               [f (hash-ref ft (string->symbol fieldName))]
               [index (field-index f)]
               ; Meow for sanity preservation also grab type info
               [fieldType (field-type f)]
               ; lookup field symbol from object field list
               ; XXX: remove any objects that don't have enough field entries
               [fieldAddrs
                 (filter identity
                   (map
                     (λ (obj)
                        (if (< index (length (object-field-symbols obj)))
                          (list-ref (object-field-symbols obj) index)
                          #f))
                     objs))])
              (begin
                (unless (equal? fieldType type) (error "types don't match in iget"))
                (let ([σ-prime (foldl (λ (fieldAddr h) (union-into-store h fieldAddr (lookup/fp h fp rv))) σ fieldAddrs)])
                  (list (state s fp σ-prime kaddr t-prime)))))]

          ; array get
          [`(,(? (opcode-predicate "aget")) ,rd ,ra ,ri)
            (let* ([arrs (lookup/fp σ fp ra)]
                   [indices (lookup/fp σ fp ri)]
                   [arr-values
                     (for*/list ([arr arrs] [index indices])
                                `(,(list-ref (array-instance arr) index) ,(array-type arr)))])
              (list (state s fp (union-into-store σ `(,rd ,fp) arr-values) kaddr t-prime)))]

          ; array put
          [`(,(? (opcode-predicate "aput")) ,rs ,ra ,ri)
            (let* ([arrs (lookup/fp σ fp ra)]
                   [indices (lookup/fp σ fp ri)]
                   [vals (D (lookup/fp σ fp rs))])
              (begin
                ; TODO: Replace commented line below with non-deterministic version!
                ;       May require restructuring of array object representation
                ; (vector-set! (array-instance arr) index val)
                (list (state s fp σ kaddr t-prime))))]

          ; static get
          [`(,(? (opcode-predicate "sget")) ,rd ,id)
            'bunk]

          ; static put
          [`(,(? (opcode-predicate "sput")) ,rd ,id)
            'bunk]

          ; Array
          ; -----------------
          ; TODO:
          ; fill-array-data filled-new-array/range filled-new-array

          [`(array-length ,rd ,rs)
            (let* ([arrs (lookup/fp σ fp rs)]
                   [lengths (map (λ (arr) 'number) arrs)]
                   [σ-prime (union-into-store σ `(,rd ,fp) lengths)])
              (list (state s fp σ-prime kaddr t-prime)))]

          [`(new-array ,rd ,rs ,type)
            'bunk]

          ; Other
          ; -----------------
          ; TODO:
          ; throw monitor-enter monitor-exit
          [`(throw ,r) '()]

          ; Types
          ; -----------------
          ; TODO:
          ; check-cast

          ; XXX: check that 'type' is same as classpath...
          [`(instance-of ,rd ,rs ,type)
            (let* ([objs (lookup/fp σ fp rs)]
                   [vals
                    (map (λ (obj)
                           (if (and (not (primitive-type? type)) (object? obj) (eq? (object-class-path obj) type))
                               'number
                               'number))
                         objs)])
              (list (state s fp (union-into-store σ `(,rd ,fp) vals) kaddr t-prime)))]


          ; Unary/Binary Operations
          ; -----------------
          [`(,(? unop?) ,rd ,rs)
            (let* ([vals (map (λ (x) (abstract-unop (car this-statement) x)) (lookup/fp σ fp rs))])
              (list (state s fp (union-into-store σ `(,rd ,fp) vals) kaddr t-prime)))]

          [`(,(? binop?) ,rd ,r1 ,r2)
            (let* ([xs (lookup/fp σ fp r1)]
                   [ys (lookup/fp σ fp r2)]
                   [results
                     (for*/list ([x xs] [y ys])
                                (abstract-binop (car this-statement) x y))])
              (list (state s fp (union-into-store σ `(,rd ,fp) results) kaddr t-prime)))]

          [`(,(? binop/lit?) ,rd ,r1 ,lit)
            (let* ([vals (map (λ (x) (abstract-binop (car this-statement) x lit)) (lookup/fp σ fp r1))])
              (list (state s fp (union-into-store σ `(,rd ,fp) vals) kaddr t-prime)))]

          [`(,(? binop/2addr?) ,rd ,r1)
            (let* ([xs (lookup/fp σ fp rd)]
                   [ys (lookup/fp σ fp r1)]
                   [results
                     (for*/list ([x xs] [y ys])
                                (abstract-binop (car this-statement) x y))])
              (list (state s fp (union-into-store σ `(,rd ,fp) results) kaddr t-prime)))]

          ; Operations with no state change.
          ; -----------------
          ; just do next computation
          [(? nop?) (next metas (state s fp σ kaddr t-prime))]

      (error 'next (format "Expected a state struct, got ~a" current-state))))
    (error "Not a state struct")))

; -------------------------------------
; Misc.
; -------------------------------------
(define (nop? stmt)
  (match stmt
         [(or `(limit registers ,_)
              `(line ,_)
              `(label ,_)
              `(catch ,_ from ,_ to ,_ using ,_)
              `(var ,_ ,_ ,_ ,_ ,_)
              '(nop)) #t]
         [otherwise #f]))

; invoke helper
; lookup method and map arguments into new fp
(define (invoke-method metas fp σ kaddr t is-direct rlst id types obj)
  (let ([virtual-method (virtual-lookup metas (method-key id types) (object-class-path obj) is-direct)]
        [fp-prime (alloc t)])
    (if (method? virtual-method)
      (let*
        ; compute argument parameter mapping:
        ;   parameter arguments are shift to the end of the
        ;   registers available in the method being invoked.
        ([register-limit (method-reg-limit virtual-method)]
         [start-register (- register-limit (length rlst))]
         [rlst-mapping
           (for/list ([r rlst]
                      [n (build-list (length rlst) (λ (x) (+ start-register x)))])
                     `((,(print-register n) ,fp-prime) ,(lookup/fp σ fp r)))]
         [σ-prime (foldl (λ (kv h) (union-into-store h (first kv) (second kv))) σ rlst-mapping)]
         ; store kont address
         [s-prime (method-statements virtual-method)])
        (begin
          ; avoid registers that look like v-1, this means error in (limit register x)
          (when (< start-register 0) (error "A negative register was created"))
          (state s-prime fp-prime σ-prime kaddr t)))
      #f)))


; Store lookup
; Lookup when frame pointer needed
(define (lookup/fp σ fp v)
  (if (hash-has-key? σ `(,v ,fp))
    (let ([val (hash-ref σ `(,v ,fp))])
      (if (set? val)
        (set->list val)
        val))
    (begin
      (pretty-print σ)
      (error (format "Failed looking up ~a in store" `(,v ,fp))))))

(define (union-into-store σ addr val)
  ; turn val into a set
  (let ([set-val (cond
                   [(set? val) val]
                   [(list? val) (list->set val)]
                   [else (set val)])])
    ; union/add it into the store
    (if (hash-has-key? σ addr)
      (let ([current-value (hash-ref σ addr)])
        (if (set? current-value)
          (hash-set σ addr (set-union current-value set-val))
          (error 'update-store "There is a value in the store, but it isn't a set: ~e" current-value)))
      (hash-set σ addr set-val))))


; For fields in object:
;  Lookup store symbol and store init value under that symbol in the store.
(define (object-init metas cls obj σ)
  (let* ([c (class-lookup metas cls)]
         [field-symbols-list (object-field-symbols obj)]
         [field-hash (class-field-table c)])
      (for/fold
        ([σ σ])
        ([elem (hash->list field-hash)])
        (let ([store-sym (list-ref field-symbols-list (field-index (cdr elem)))])
          (match (field-type (cdr elem))
                 ['int (union-into-store σ store-sym 'number)]
                 ['(object java/lang/String) (union-into-store σ store-sym dummy-string)]
                 ; TODO: finish inits for more types
                 [_ σ])))))

(define (opcode-predicate init)
  (λ (v) (regexp-match (string-append "^" init) (symbol->string v))))

(define (print-register n)
  (string->symbol (string-join (list "v" (number->string n)) "")))


; -------------------------------------
; Abstract Unary/Binary Operations
; -------------------------------------
(define (abstract-unop op num)
  'number)

(define (abstract-binop op lhs rhs)
  (match op
    [(or 'div-int 'div-long 'div-float 'div-double 'rem-int 'rem-long 'rem-float 'rem-double) (abstract-divide lhs rhs)]
    [else 'number]))

; copied from Petey...
(define (abstract-divide lhs rhs)
  (match rhs
    [(== 'number) (error "could divide by zero")]
    [(and (? number?) (== 0)) (error "divides by zero")]
    [(? number?) 'number]
    [(and (? set?) (? (λ (v) (set-member? v 0)))) (error "could divide by zero")]
    [(? set?) 'number]))

; -------------------------------------
; Unary/Binary Operations
; -------------------------------------
; TODO: Perform unary operation
(define (unop op-str x)
  (let
    ([val (prim-value x)])
    (match op-str
           [(or 'neg-int 'neg-long 'neg-float 'neg-double) (prim (- val) (prim-type x))]
           [(or 'not-int 'not-long) (prim (if (eq? val 0) #t #f) 'bool)]
           [_ x]))) ; TODO: finish cast opts

; TODO: Perform binary operation
(define (binop op-str x y)
  (match op-str
         ['add-int/lit8 (prim (+ (prim-value x) (binary->decimal (decimal->binary y) #t 8)) (prim-type x))]
         ['add-int (prim (+ (prim-value x) (prim-value y)) (prim-type x))]
         ['mul-int/2addr (prim (* (prim-value x) (prim-value y)) (prim-type x))]
         ['mul-int/lit-8 (prim (* (prim-value x) (binary->decimal (decimal->binary y) #t 8)) (prim-type x))]
         [_ (error (format "binop dispatch for ~a not found/complete" op-str))]))

; if-eq if-ne if-lt if-ge if-gt if-le
(define (if-dispatch op x y stmt1 stmt2)
  (match op
         ['if-eq (if (eq? x y) stmt1 stmt2)]
         ['if-ne (if (not (eq? x y)) stmt1 stmt2)]
         ['if-lt (if (< x y) stmt1 stmt2)]
         ['if-le (if (<= x y) stmt1 stmt2)]
         ['if-ge (if (>= x y) stmt1 stmt2)]
         ['if-gt (if (> x y) stmt1 stmt2)]))

; if-eqz if-nez if-ltz if-gez if-gtz if-lez
(define (ifz-dispatch op x stmt1 stmt2)
  (match op
         ['if-eqz (if (eq? x 0) stmt1 stmt2)]
         ['if-nez (if (not (eq? x 0)) stmt1 stmt2)]
         ['if-ltz (if (< x 0) stmt1 stmt2)]
         ['if-gez (if (<= x 0) stmt1 stmt2)]
         ['if-gtz (if (< x 0) stmt1 stmt2)]
         ['if-lez (if (>= x 0) stmt1 stmt2)]))

; Perform the indicated floating point or long comparison, storing 0 if the two arguments are equal, 1 if the second argument is larger, or -1 if the first argument is larger.
; TODO bias stuff:
; The "bias" listed for the floating point operations indicates how NaN comparisons are treated: "Gt bias" instructions return 1 for NaN comparisons, and "lt bias" instructions return -1.
; For example, to check to see if floating point a < b, then it is advisable to use cmpg-float; a result of -1 indicates that the test was true, and the other values indicate it was false either due to a valid comparison or because one or the other values was NaN.
(define (cmp-dispatch op x y)
  (match op
         ; lt bias
         ['cmpl-float (cond [(eq? x y) 0]
                            [(> x y) 1]
                            [else -1])]
         ; gt bias
         ['cmpg-float (cond [(eq? x y) 0]
                            [(> x y) 1]
                            [else -1])]
         ; lt bias
         ['cmpl-double (cond [(eq? x y) 0]
                             [(> x y) 1]
                             [else -1])]
         ; gt bias
         ['cmpg-double (cond [(eq? x y) 0]
                             [(> x y) 1]
                             [else -1])]

         ['cmp-long (cond [(eq? x y) 0]
                          [(> x y) 1]
                          [else -1])]))


; -------------------------------------
; Init & Run
; -------------------------------------
(define (load-program directory init-function)
  (let* ([files (filter (λ (f) (regexp-match #rx".+\\.sexpr$" f)) (directory-list directory))]
         [absolute-files (map (λ (f) (build-path directory f)) files)]
         [metas (build-metas absolute-files)]
         [init-stmts (method-statements (method-lookup metas init-function))]
         [init-state
           (state init-stmts
                  'firstFP
                  (make-immutable-hash `((0 . ,(set 'halt))))
                  0
                  (build-list k values))])
    (values init-state metas)))

; -------------------------------------
; Explore Call-Sites
; -------------------------------------
; create list of visited states
(define (visit-states metas queue visited)
  (if (null? queue)
    visited
    (let* ([current-state (car queue)]
           [next-states (gc-next metas current-state)]
           [new-queue
             (for/fold
               ([q (cdr queue)])
               ([state next-states])
               (if (member state visited) q (cons state q)))])
      (visit-states metas new-queue (cons current-state visited)))))

(define (visit-states-set cnt metas queue visited [gc? #f])
  (if (null? queue)
    visited
    (let* ([current-state (car queue)]
           [transition-function (if gc? gc-next next)]
           [next-states (transition-function metas current-state)]
           [new-queue
             (for/fold
               ([q (cdr queue)])
               ([state next-states])
               (if (set-member? visited state) q (cons state q)))])
      (when (equal? 0 (modulo cnt 100)) (displayln cnt))
      (visit-states-set (add1 cnt) metas new-queue (set-add visited current-state) #t))))

; create .dot and .html files
(define (make-graph visited metas st-id-map)
  (visited->graph visited (λ (s) (gc-next metas s)) st-id-map))

; -------------------------------------
; Debugging/Interactive Experimentation
; -------------------------------------
(define app-directory "/home/mates/ucomb/tapas/example_code/LambdaInterpreterApp/dedexOut/com/android/demo/lambad")
(define starting-function 'com/android/demo/lambad/LambadActivity/fixedEval)
(define-values
    (s m) (load-program app-directory starting-function))

; do state exploration
(define visited-states (visit-states-set 0 m `(,s) (set) #t))
(displayln (length (set->list visited-states)))

; create state to unique id mapping for graph creation
(define st-id-map (build-state-id-mapping (set->list visited-states)))

; to build a .dot graph file from visited states
;(make-graph (set->list visited-states) m st-id-map)
