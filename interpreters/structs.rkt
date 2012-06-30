#lang racket

(provide (struct-out class) (struct-out method) (struct-out field)
         (struct-out meta) (struct-out prim) (struct-out array)
         (struct-out state) (struct-out object) (struct-out funk))

; -------------------------------------
; Structs
; -------------------------------------
(struct state (statements frame-pointer store kaddr time) #:prefab)
(struct funk (successors frame-pointer kaddr) #:prefab)

(struct class (field-table method-table path super attrs))
(struct method (return-type arg-types attr reg-limit statements))
(struct field (type attr index))

(struct meta (classes labels methods objects))

(struct object (field-symbols class-path) #:transparent)
(struct array (instance type))
(struct prim (value type) #:prefab)
