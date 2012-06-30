#lang racket

(provide (all-defined-out))

(require "structs.rkt")
(require "utils.rkt")

; -------------------------------------
; Meta-functions lookups
; -------------------------------------

; Objs : ClassPath -> Object
(define (object-lookup meta-funcs cp)
  (let ([object-table (meta-objects meta-funcs)])
    (if (hash-has-key? object-table cp)
      (hash-ref object-table cp)
      (error "Object Table" (format "Failed to lookup ~a" cp)))))

; CT : ClassPath -> Class
(define (class-lookup meta-funcs cp)
  (let ([class-table (meta-classes meta-funcs)])
    (if (hash-has-key? class-table cp)
      (hash-ref class-table cp)
      (error "Class Table" (format "Failed to lookup ~a" cp)))))

; S : Label -> Stmt*
(define (label-lookup meta-funcs l)
  (let ([label-table (meta-labels meta-funcs)])
    (if (hash-has-key? label-table l)
      (hash-ref label-table l)
      (error "Label Table" (format "Failed to lookup ~a" l)))))

; M : Id -> method struct
(define (method-lookup meta-funcs i)
  (let ([method-table (meta-methods meta-funcs)])
    (if (hash-has-key? method-table i)
      (hash-ref method-table i)
      (error "Method Table" (format "Failed to lookup ~a" i)))))

; V : Id x Object -> method struct
; returns method or false if lookup failure
(define (virtual-lookup meta-funcs i cp [is-direct? #f])
  (let* ([ct (meta-classes meta-funcs)]
         [class-instance
           (if (hash-has-key? ct cp)
             (hash-ref ct cp)
             #f)])
    (if (not class-instance)
      #f  ; Failed to load class
      (let ([mt (class-method-table class-instance)])
        (if (hash-has-key? mt i)
          (hash-ref mt i)
          ; do super lookup
          (cond
            ; XXX: skip Object inits
            [(equal? "java/lang/Object/<init>" (symbol->string i))
             (method 'bunk 'bunk 'bunk 1 '((return-void)))]

            ; if this is a virtual lookup, try looking in a parent class
            ; also treat direct lookups of <inits> as virtual.
            [(or
               (and (not is-direct?)
                    (not (null? (class-super class-instance))))
               (regexp-match #rx".+\\<init\\>$" (symbol->string i)))
             (let* ([super (class-super class-instance)]
                    [super-id (get-super-id super i)])
               (virtual-lookup meta-funcs super-id super))]

            ; failed to lookup method
            [else #f]))))))

; D
(define (D v)
  (cond [(prim? v) (prim-value v)]))

; Helper
(define (get-super-id super id)
  (let ([id (symbol->string id)])
    ; Does the method name have type info delimited by '*'?
    (if (member #\* (string->list id))
      (let* ([split-on-first-type (regexp-split #px"\\*" id)]
             [path (regexp-split #px"/" (car split-on-first-type))]
             [method (substring id
                                (- (string-length (car split-on-first-type))
                                   (string-length (last path))))])
        (join-symbols super (string->symbol method)))

      (let ([path (regexp-split #px"/" id)])
        (join-symbols super (string->symbol (last path)))))))
