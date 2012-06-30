#lang racket

; ClassTable related functions for Dalvik VM
; Chomps on dex2sex output

(provide build-metas get-inherited-fields method-key)
(require test-engine/racket-tests)

(require "structs.rkt")
(require "utils.rkt")
(require "dalvik-utils.rkt")

; Build meta-function struct
(define (build-metas files)
  (let* ([method-table (build-global-static-table files (make-immutable-hash))]
         [class-table (build-class-table files (make-immutable-hash))]
         [flattened-class-table (flatten-class-table-fields class-table)]
         [label-table (build-global-label-table files (make-immutable-hash))]
         [object-table (build-object-table
                         flattened-class-table
                         (hash-keys flattened-class-table)
                         (make-immutable-hash))])
    (meta flattened-class-table label-table method-table object-table)))

; -------------------------------------
; Build Instantiated Object Table
; -------------------------------------
(define (build-object-table class-table class-queue object-table)
  (if (null? class-queue)
    object-table
    (let* ([class-path (car class-queue)]
           [clss (hash-ref class-table class-path)]
           [field-syms (for/list ([n (hash-count (class-field-table clss))]) (gensym))]
           [object-instance (object field-syms class-path)]
           [new-object-table (hash-set object-table class-path object-instance)])
      (build-object-table class-table (cdr class-queue) new-object-table))))

; -------------------------------------
; Build Method/Field Tables
; -------------------------------------
(define (build-field-table sexpr ft index)
  (match sexpr
    [`() ft]
    [`((field ,name ,type) ,rest ...)
     (build-field-table rest (hash-set ft name (field type '() index)) (+ 1 index))]
    [`((field (attrs ,attrs ...) ,name ,type) ,rest ...)
     (build-field-table rest (hash-set ft name (field type attrs index)) (+ 1 index))]
    [`(,hd ,rest ...) (build-field-table rest ft index)]))

(define (build-method-table sexpr methodTable class-name)
  (match sexpr
    [`() methodTable]
    [`((method (attrs ,attrs ...) ,name ,arg-types ,return-type (limit registers ,limit) ,stmts ...) ,rest ...)
      (if (not (member 'static attrs))
        ; non-static methods
        (let* ([methodTable- (hash-set methodTable (join-symbols class-name (method-key name arg-types)) (method return-type arg-types attrs limit stmts))])
         (build-method-table rest methodTable- class-name))
        ; skip static methods
        (build-method-table rest methodTable class-name))]
    [`(,hd ,rest ...) (build-method-table rest methodTable class-name)]))

; -------------------------------------
; Build Class Tables
; -------------------------------------
(define (build-class-struct sexpr)
  (match sexpr
    ; TODO: store additional class info: super, source, and attrs
    [`(class ,attrs ,cp (super ,super) ,source ,rest ...)
     (let*
       ([ft (build-field-table rest (make-immutable-hash) 0)]
        [attrs (match (car rest)
                      [`(implements ,interface) `(,(car rest))]
                      [otherwise '()])]
        ; TODO: make a design decision as to what to do with labelTable
        [methodTable (build-method-table rest (make-immutable-hash) cp)])

       (class ft methodTable cp super attrs))]
    ; TODO: deal with interfaces
    [`(interface ...) #f]
    (error "not a class sexpr")))

(define (build-class-table dex-files ct)
  (match dex-files
    [`() ct]
    [`(,filepath ,rest ...)
      (let* ([sexpr (file->sexpr filepath)]
             [c (build-class-struct sexpr)])
        (if (class? c)
          (build-class-table rest (hash-set ct (class-path c) c))
          (build-class-table rest ct)))]))

; Flatten entire class-table so that classes copy inherited fields
; XXX: the following flattening functions are algorithmically terrible 0_o
(define (flatten-class-table-fields class-table)
  (for/fold
    ([ct class-table])
    ([elem (hash->list class-table)])
    (hash-set ct (car elem) (flatten-inherited-fields (car elem) (cdr elem) class-table))))

; modify field table of a given class struct to include fields from parent classes
(define (flatten-inherited-fields class-name class-struct class-table)
  (if (library-class? class-name)
    class-struct  ; don't try to look into library classes
    (let* ([inherited-fields (get-inherited-fields class-table class-name)]
           ; reset indexes in field structs
           [flattened-fields
             (for/fold
               ([ft (make-immutable-hash)])
               ([elem inherited-fields]
                [n (build-list (length inherited-fields) values)])
               (hash-set ft (car elem) (struct-copy field (cdr elem) [index n])))]
           [current-class (hash-ref class-table class-name)])
      (struct-copy class class-struct [field-table flattened-fields]))))

; returns list of all fields a class has & inherits
(define (get-inherited-fields class-table class-name)
   (let* ([current-class (hash-ref class-table class-name)]
          [ft-list (hash->list (class-field-table current-class))]
          [super (class-super current-class)])
    (if (library-class? super)
      ft-list
      (append ft-list (get-inherited-fields class-table super)))))


; -------------------------------------
; Build Static Table
; -------------------------------------
; build static table out of multiple dex files
(define (build-global-static-table dex-files staticTable)
  (match dex-files
    [`() staticTable]
    [`(,filepath ,rest ...)
      (let* ([sexpr (file->sexpr filepath)]
             [class-name (caddr sexpr)]
             [staticTable- (build-local-static-table sexpr staticTable class-name)])
        (build-global-static-table rest staticTable-))]))

; build static table out of one dex file sexpr
(define (build-local-static-table sexpr staticTable class-name)
  (match sexpr
    [`() staticTable]
    [`((method (attrs ,attrs ...) ,name ,arg-types ,return-type (limit registers ,limit) ,stmts ...) ,rest ...)
      (if (not (member 'static attrs))
        ; not static
        (build-local-static-table rest staticTable class-name)
        ; is static
        (let*
          ([staticTable-
             (hash-set staticTable
                       (join-symbols class-name name)
                       (method return-type arg-types attrs limit stmts))])
          (build-local-static-table rest staticTable- class-name)))]

    [`(,hd ,rest ...) (build-local-static-table rest staticTable class-name)]))

; -------------------------------------
; Build Label Table
; -------------------------------------
; build label table out of multiple dex files
(define (build-global-label-table dex-files labelTable)
  (match dex-files
    [`() labelTable]
    [`(,filepath ,rest ...)
      (let* ([sexpr (file->sexpr filepath)]
             [methods (filter (λ (expr) (match expr [`(method ,rest ...) #t] [_ #f])) (cdr sexpr))]
             ; for each method, construct label table
             [labelTables (map (curry build-label-table (make-immutable-hash)) methods)]
             ; merge into one label table
             [mergedLableTables (mergeHashes (cons labelTable labelTables))])
        (build-global-label-table rest mergedLableTables))]))

; build label table out of one dex file sexpr
(define (build-label-table lt sexpr)
  (match sexpr
    [`() lt]
    [`((label ,l) ,rest ...) (build-label-table (hash-set lt l rest) rest)]
    [`(,hd ,rest ...) (build-label-table lt rest)]))

; -------------------------------------
; Helper Funcs
; -------------------------------------
(define (file->sexpr f)
  (car (port->list (open-input-file f))))

(define (method-key name arg-types)
  (let ([with-types
          (foldl
            (λ (type acc) (string-append acc "*" (type->string type)))
            (symbol->string name)
            arg-types)])
    (string->symbol with-types)))

(define (type->string type)
  (match type
    [`(object ,obj) (symbol->string obj)]
    [_ (symbol->string type)]))

(define (port->list port)
  (let ([next (read port)])
    (if (eof-object? next)
        '()
        (cons next (port->list port)))))

(define (mergeHashes hashes)
  (for*/fold ([new-hash (hash)])
             ([current-hash hashes]
              [(k v) current-hash])
             (hash-set new-hash k v)))


; -------------------------------------
; Tests
; -------------------------------------
; TODO: use relative file paths...
(define files
  '("/home/mates/ucomb/tapas/interpreters/class-example-1.sexpr"
    "/home/mates/ucomb/tapas/interpreters/class-example-2.sexpr"))

(define class-example-1-sexpr (file->sexpr (car files)))

; label table
(define lt (build-global-label-table files (make-immutable-hash)))

(check-expect (hash-has-key? lt 'le54) #t)
(check-expect (and (hash-has-key? lt 'le96) (eq? (hash-ref lt 'le96)
  '((line 82) (return-void)))) #t)

; static table
(define st (build-global-static-table files (make-immutable-hash)))
(check-expect (hash-has-key? st 'quxed) #t)

; method table
(define mt (build-method-table class-example-1-sexpr (make-immutable-hash) 'com/android/demo/notepad3/NoteEdit))
(check-expect (hash-has-key? mt 'com/android/demo/notepad3/NoteEdit/populateFields) #t)

; field table
(define ft (build-field-table class-example-1-sexpr (make-immutable-hash) 0))
(check-expect (hash-has-key? ft 'anArray) #t)

; class table
(define ct (build-class-table files (make-immutable-hash)))
(check-expect (hash-has-key? ct 'com/android/demo/notepad3/Foo) #t)

; check that populateFields in mt and ct are the same
(check-expect
  (eq?
    (hash-ref
      (class-method-table (hash-ref ct 'com/android/demo/notepad3/Foo))
      'populateFields)
    (hash-ref mt 'populateFields))
  #t)
