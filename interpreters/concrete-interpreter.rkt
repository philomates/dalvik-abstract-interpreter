#lang racket

; Concrete interpreter for the Dalvik VM
; Chomps on dex2sex output

; meta-function lookup functions
(require "meta-functions.rkt")
; build class, static method, & label tables
(require "meta-function-builder.rkt")
; low level numeric functions
(require "utils.rkt")
; functions to help classify instructions
(require "dalvik-utils.rkt")

(require "structs.rkt")

; State: [Stmt] -> {fp} -> [((fp, reg), (value, type))] -> funk
(struct cstate (statements frame-pointer store kont) #:prefab)
(struct funk (successors frame-pointer kont) #:prefab)


; -------------------------------------
; Allocation
; -------------------------------------
(define (alloc)
  (gensym))

; -------------------------------------
; Transition Function
; -------------------------------------
(define (next metas current-state)
  (if (cstate? current-state)
      (let* ([current-statements (cstate-statements current-state)]
             [fp (cstate-frame-pointer current-state)]
             [σ (cstate-store current-state)]
             [k (cstate-kont current-state)]
             [v-exception 'bunk]
             [s (rest current-statements)]
             [this-statement (first current-statements)])
        (match this-statement

          ; Move
          ; -----------------
          [`(move-exception ,r)
           (let ([σ-prime (hash-set σ `(,r ,fp) (lookup/fp σ fp v-exception))])
             (cstate s fp σ-prime k))]

          [`(move-result ,r)
           (let ([σ-prime (hash-set σ `(,r ,fp) (hash-ref σ 'v-result))])
             (cstate s fp σ-prime k))]

          [`(move-object ,rd ,rs)
           (let ([σ-prime (hash-set σ `(,rd ,fp) (lookup/fp σ fp rs))])
             (cstate s fp σ-prime k))]

          [`(move-result-object ,r)
           (let ([σ-prime (hash-set σ `(,r ,fp) (hash-ref σ 'v-result))])
             (cstate s fp σ-prime k))]

          ; Return
          ; -----------------
          ['(return-void)
           (match k
             [(funk s-prime fp-prime k-prime)
              (cstate s-prime fp-prime σ k-prime)])]

          [`(return ,r)
           (match k
             [(funk s-prime fp-prime k-prime)
              (let ([σ-prime (hash-set σ 'v-result (lookup/fp σ fp r))])
                (cstate s-prime fp-prime σ-prime k-prime))])]

          [`(return-object ,r)
           (match k
             [(funk s-prime fp-prime k-prime)
              (let ([σ-prime (hash-set σ 'v-result (lookup/fp σ fp r))])
                (cstate s-prime fp-prime σ-prime k-prime))])]

          ; Const
          ; -----------------
          [`(const-string ,vx ,const)
           (let ([σ-prime (hash-set σ `(,vx ,fp) (prim const '(object java/lang/String)))])
             (cstate s fp σ-prime k))]

          [`(,(? (opcode-predicate "const")) ,vx ,const)
           (let ([σ-prime (hash-set σ `(,vx ,fp) (prim const 'int))])
             (cstate s fp σ-prime k))]

          ; Switch
          ; -----------------
          ; TODO:
          ; sparse-switch packed-switch

          [`(goto ,l)
           (cstate (label-lookup metas l) fp σ k)]

          ; Compare
          ; -----------------
          ; cmpl-float cmpg-float cmpl-double cmpg-double cmp-long
          [`(,(? (opcode-predicate "cmp")) ,rd ,r0 ,r1)
            (let* ([result (cmp-dispatch
                             (car this-statement)
                             (D (lookup/fp σ fp r0))
                             (D (lookup/fp σ fp r1)))]
                   [σ-prime (hash-set σ `(,rd ,fp) result)])
              (cstate s fp σ-prime k))]



          ; Branches
          ; -----------------
          ; if-eq if-ne if-lt if-ge if-gt if-le
          [`(,(? (opcode-predicate "if-")) ,r0 ,r1 ,l)
            (if-dispatch
              (car this-statement)
              (D (lookup/fp σ fp r0))
              (D (lookup/fp σ fp r1))
              (cstate (label-lookup metas l) fp σ k)
              (cstate s fp σ k))]

          ; if-eqz if-nez if-ltz if-gez if-gtz if-lez
          [`(,(? (opcode-predicate "if-")) ,r ,l)
            (ifz-dispatch
              (car this-statement)
              (D (lookup/fp σ fp r))
              (cstate (label-lookup metas l) fp σ k)
              (cstate s fp σ k))]

          ; Invokes
          ; -----------------

          ; Same as invoke-virtual but throw error instead of climbing inheritance ladder
          ; if virtual-lookup doesn't find the method.
          [`(invoke-direct ,rlst ,id)
            (let* ([k-prime (funk s fp k)]
                   [fp-prime (alloc)]
                   [obj (lookup/fp σ fp (car rlst))]
                   [direct-method (virtual-lookup metas id (object-class-path obj) #t)]
                   ; compute argument parameter mapping:
                   ;   parameter arguments are shift to the end of the
                   ;   registers available in the method being invoked.
                   [register-limit (method-reg-limit direct-method)]
                   [start-register (- register-limit (length rlst))]
                   [rlst-mapping
                     (for/list ([r rlst]
                                [n (build-list (length rlst) (λ (x) (+ start-register x)))])
                               `((,(print-register n) ,fp-prime) ,(lookup/fp σ fp r)))]
                   [σ-prime (foldl (λ (kv h) (hash-set h (car kv) (cadr kv))) σ rlst-mapping)]
                   [s-prime (method-statements direct-method)])
              (cstate s-prime fp-prime σ-prime k-prime))]

          [(or `(invoke-virtual ,rlst ,id)
               `(invoke-super ,rlst ,id))
            (let* ([k-prime (funk s fp k)]
                   [fp-prime (alloc)]
                   [obj (lookup/fp σ fp (car rlst))]
                   [virtual-method (virtual-lookup metas id (object-class-path obj))]
                   ; compute argument parameter mapping:
                   ;   parameter arguments are shift to the end of the
                   ;   registers available in the method being invoked.
                   [register-limit (method-reg-limit virtual-method)]
                   [start-register (- register-limit (length rlst))]
                   [rlst-mapping
                     (for/list ([r rlst]
                                [n (build-list (length rlst) (λ (x) (+ start-register x)))])
                               `((,(print-register n) ,fp-prime) ,(lookup/fp σ fp r)))]
                   [σ-prime (foldl (λ (kv h) (hash-set h (car kv) (cadr kv))) σ rlst-mapping)]
                   [s-prime (method-statements virtual-method)])
              (cstate s-prime fp-prime σ-prime k-prime))]

          [`(invoke-static ,rlst ,id)
            (let* ([k-prime (funk s fp k)]
                   [fp-prime (alloc)]
                   [static-method (method-lookup metas id)]
                   ; compute argument parameter mapping:
                   ;   parameter arguments are shift to the end of the
                   ;   registers available in the method being invoked.
                   [register-limit (method-reg-limit static-method)]
                   [start-register (- register-limit (length rlst))]
                   [rlst-mapping
                     (for/list ([r rlst]
                                [n (build-list (length rlst) (λ (x) (+ start-register x)))])
                               `((,(print-register n) ,fp-prime) ,(lookup/fp σ fp r)))]
                   [σ-prime (foldl (λ (kv h) (hash-set h (car kv) (cadr kv))) σ rlst-mapping)]
                   [s-prime (method-statements static-method)])
              (begin
                (print rlst-mapping)
                (cstate s-prime fp-prime σ-prime k-prime)))]

          ; TODO: ???
          [`(invoke-interface ...)
            ('bunk)]

          ; TODO:
          ; invoke-virtual/range invoke-super/range invoke-direct/range
          ; invoke-static/range invoke-interface/range

          [`(new-instance ,r ,type)
           (let* ([obj (new-object metas type)]
                  [σ-with-inits (object-init metas type obj σ)]
                  [σ-prime (hash-set σ-with-inits `(,r ,fp) obj)])
             (cstate s fp σ-prime k))]

          ; gets/puts
          ; -----------------
          ; instance get
          [`(,(? (opcode-predicate "iget")) ,rd ,rs ,id ,type)
            (match-let*
              ([obj (lookup/fp σ fp rs)]
               ; grab field index from field-table in class-table
               [(list classPath fieldName) (regexp-split #px"\\." (symbol->string id))]
               [ft (class-field-table (class-lookup metas (string->symbol classPath)))]
               [f (hash-ref ft (string->symbol fieldName))]
               [index (field-index f)]
               ; Meow, for sanity preservation also grab type info
               [fieldType (field-type f)]
               ; lookup field symbol from object field list
               [fieldVar (list-ref (object-field-symbols obj) index)]
               ; set rd to value in field symbol
               [σ-prime
                 (hash-set σ `(,rd ,fp) (hash-ref σ fieldVar))])
              (begin
                (unless (equal? fieldType type) (error "types don't match in iget"))
                (cstate s fp σ-prime k)))]

          ; instance put
          [`(,(? (opcode-predicate "iput")) ,rv ,rs ,id ,type)
            (match-let*
              ([obj (lookup/fp σ fp rs)]
               ; grab field index from field-table in class-table
               [(list classPath fieldName) (regexp-split #px"\\." (symbol->string id))]
               [ft (class-field-table (class-lookup metas (string->symbol classPath)))]
               [f (hash-ref ft (string->symbol fieldName))]
               [index (field-index f)]
               ; Meow for sanity preservation also grab type info
               [fieldType (field-type f)]
               ; lookup field symbol from object field list
               [fieldVar (list-ref (object-field-symbols obj) index)]
               ; set field symbol to rv
               [σ-prime (hash-set σ fieldVar (lookup/fp σ fp rv))])
              (begin
                (unless (equal? fieldType type) (error "types don't match in iget"))
                (cstate s fp σ-prime k)))]

          ; array get
          [`(,(? (opcode-predicate "aget")) ,rd ,ra ,ri)
            (let* ([arr (lookup/fp σ fp ra)]
                   [index (lookup/fp σ fp ri)]
                   [val (list-ref (array-instance arr) index)]
                   [type (array-type arr)]
                   [σ-prime (hash-set σ `(,rd ,fp) `(,val ,type))])
              (cstate s fp σ-prime k))]

          ; array put
          [`(,(? (opcode-predicate "aput")) ,rs ,ra ,ri)
            (let* ([arr (lookup/fp σ fp ra)]
                   [index (lookup/fp σ fp ri)]
                   [val (D (lookup/fp σ fp rs))])
              (begin
                (vector-set! (array-instance arr) index val)
                (cstate s fp σ k)))]

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
            (let* ([arr (lookup/fp σ fp rs)]
                   [σ-prime (hash-set σ `(,rd ,fp) (prim (vector-length (array-instance arr)) 'int))])
              (cstate s fp σ-prime k))]

          [`(new-array ,rd ,rs ,type)
            'bunk]

          ; Other
          ; -----------------
          ; TODO:
          ; throw monitor-enter monitor-exit

          ; Types
          ; -----------------
          ; TODO:
          ; check-cast

          ; XXX: check that 'type' is same as classpath...
          [`(instance-of ,rd ,rs ,type)
            (let ([obj (lookup/fp σ fp rs)])
              (if (and (not (primitive-type? type)) (object? obj) (eq? (object-class-path obj) type))
                 (cstate s fp (hash-set σ `(,rd ,fp) (prim 1 'int)))
                 (cstate s fp (hash-set σ `(,rd ,fp) (prim 0 'int)))))]


          ; Unary/Binary Operations
          ; -----------------
          [`(,(? unop?) ,rd ,rs)
            (let* ([v (unop (car this-statement) (lookup/fp σ fp rs))]
                   [σ-prime (hash-set σ `(,rd ,fp) v)])
              (cstate s fp σ-prime k))]

          [`(,(? binop?) ,rd ,r1 ,r2)
            (let* ([v (binop (car this-statement) (lookup/fp σ fp r1) (lookup/fp σ fp r2))]
                   [σ-prime (hash-set σ `(,rd ,fp) v)])
              (cstate s fp σ-prime k))]

          [`(,(? binop/lit?) ,rd ,r1 ,lit)
            (let* ([v (binop (car this-statement) (lookup/fp σ fp r1) lit)]
                   [σ-prime (hash-set σ `(,rd ,fp) v)])
              (cstate s fp σ-prime k))]

          [`(,(? binop/2addr?) ,rd ,r1)
            (let* ([v (binop (car this-statement) (lookup/fp σ fp rd) (lookup/fp σ fp r1))]
                   [σ-prime (hash-set σ `(,rd ,fp) v)])
              (cstate s fp σ-prime k))]


          ; TODO: use the following
          ; -----------------
          [`(catch ,exception from ,start-label to ,end-label using ,exception-label) (cstate s fp σ k)]
          [`(var ,r1 ,var-name ,type ,start-label ,end-label) (cstate s fp σ k)]

          ; Operations with no cstate change.
          ; -----------------
          [`(limit registers ,n) (cstate s fp σ k)]
          [`(line ,n) (cstate s fp σ k)]
          [`(label ,n) (cstate s fp σ k)]
          ['(nop) (cstate s fp σ k)]


      (error 'next (format "Expected a cstate struct, got ~a" current-state))))
    (error "Not a cstate struct")))

; -------------------------------------
; Misc.
; -------------------------------------
; Store lookup
; Lookup when frame pointer needed
(define (lookup/fp σ fp v)
  (hash-ref σ `(,v ,fp)))

; build new object from class
; essentially just create a list of unique symbols that can be used in the store
(define (new-object metas cls)
  (let* ([c (class-lookup metas cls)]
         [fieldSyms (for/list ([n (hash-count (class-field-table c))]) (gensym))])
    (object fieldSyms (class-path c))))

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
                 ['int (hash-set σ store-sym (prim 0 'int))]
                 ; TODO: finish inits for more types
                 [_ σ])))))

(define (opcode-predicate init)
  (λ (v) (regexp-match (string-append "^" init) (symbol->string v))))

(define (print-register n)
  (string->symbol (string-join (list "v" (number->string n)) "")))



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
; Tests
; -------------------------------------
; TODO


; -------------------------------------
; Init & Run
; -------------------------------------
(define (load-program directory init-function)
  (let* ([files (filter (λ (f) (regexp-match #rx".+\\.sexpr$" f)) (directory-list directory))]
         [absolute-files (map (λ (f) (build-path directory f)) files)]
         [metas (build-metas absolute-files)]
         [init-stmts (method-statements (method-lookup metas init-function))]
         [init-state (cstate init-stmts (alloc) (make-immutable-hash) '(foo halt))])
    (values init-state metas)))

(define (eval metas cstate)
  (let ([step (next metas cstate)])
    (if (eq? 'halt (cstate-kont step))
      step
      (eval metas step))))

#|
(define run-program
  (let*-values
    ([(state0 metas) (load-program "/home/mates/home_repo/static_analysis/android/code_examples/malApp/dedexOut/com/android/demo/notepad3"
                                   'com/android/demo/notepad3/NoteEdit/runFib)])
    (eval metas state0)))
|#

(define-values
    (current-state meta-funcs) (load-program
           "/home/mates/ucomb/tapas/example_code/LambdaInterpreterApp/dedexOut/com/android/demo/lambad"
           'com/android/demo/lambad/LambadActivity/fixedEval))


; step through the program loaded
(define (step [size 1])
  (if (eq? size 1)
      (begin
        (set! current-state (next meta-funcs current-state))
        (pretty-print (cstate-frame-pointer current-state))
        (pretty-print (cstate-statements current-state))
        (pretty-print (cstate-store current-state)))
      (begin
        (set! current-state
              (for/fold ([st current-state])
                ([n (make-list size 'foo)])
                (next meta-funcs st)))
        (pretty-print (cstate-frame-pointer current-state))
        (pretty-print (cstate-statements current-state))
        (pretty-print (cstate-store current-state)))))
