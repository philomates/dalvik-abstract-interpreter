#lang racket

(require "structs.rkt")

(provide visited->graph
         state->htmlfile
         graph-file
         build-state-id-mapping
         state->id
         id->state)

; util functions to help with graphviz graph creation

(define graph-directory "callsite-graph")
(define graph-file (string-append graph-directory "/visited-states.dot"))

; prepare directory
(define (prepare-directory)
  (if (directory-exists? graph-directory)
    #f
    (make-directory graph-directory)))

; -------------------------------------
; Graphviz building funcs
; -------------------------------------
; create graphviz .dot file from visited states
(define (visited->graph visited transition-function st-id-map)
  (prepare-directory)
  (display-lines-to-file '("digraph states {\n") graph-file #:mode 'text #:exists 'replace)
  (displayln (visited->graph-body st-id-map visited transition-function 0))
  (displayln (length visited))
  (display-lines-to-file '("}\n") graph-file #:mode 'text #:exists 'append))
  ; TODO: maybe create svg, ps, etc. here:
  ; $ dot -Tsvg visited-states.dot -o visited-states.html

; for each state in visited states, compute sucessors & write to .dot file
(define (visited->graph-body st-id-map visited transition-function c)
  (if (null? visited)
    c
    (let*
      ([current-state (car visited)]
       [next-states (transition-function current-state)]
       [hashed-current-state (state->id current-state st-id-map)]
       [hashed-states (map (λ (s) (state->id s st-id-map)) next-states)]
       [node-url-text (format
                        "\t\"~a\" [URL=\"~a.html\"]"
                        hashed-current-state
                        hashed-current-state)]
       [edge-text
         (map
           (λ (next-state) (format "\t\"~a\" -> \"~a\";" hashed-current-state next-state))
           hashed-states)])
      (begin
        (state->htmlfile current-state (state->id current-state st-id-map))
        (display-lines-to-file (cons node-url-text edge-text) graph-file #:mode 'text #:exists 'append)
        (visited->graph-body st-id-map (cdr visited) transition-function (add1 c))))))

; print state struct to a html file
(define (state->htmlfile state hashed-state)
  (let*
    ([filehash (format "~a/~a.html" graph-directory hashed-state)]
     [text (list
             (format "<b>Frame Pointer:</b> ~a<br>~n" (state-frame-pointer state))
             (format "<b>Time:</b> ~a<br>~n" (state-time state))
             (format "<b>Statements:</b> <br>~a<br><br>~n"
                     (regexp-replace*
                       "\n"
                       (pretty-format (state-statements state))
                       "<br>\n"))
             (format "<b>Continuation Address:</b> ~a <br><br>~n" (state-kaddr state))
             (format "<b>Store:</b><br>~a<br><br>~n"
                     (regexp-replace*
                       "\n"
                       (store->html (state-store state))
                       "<br>\n")))])
    (display-lines-to-file text filehash #:mode 'text #:exists 'replace)))

; -------------------------------------
; State identification functions
; -------------------------------------
(define (build-state-id-mapping states)
  (let*-values
    ([(st-id-map _)
      (for/fold
        ([st-id-map (make-immutable-hash)]
         [counter 0])
        ([st states])
        (values (hash-set st-id-map st counter) (add1 counter)))])
    st-id-map))

(define (state->id st st-id-map)
  (if (hash-has-key? st-id-map st)
    (hash-ref st-id-map st)
    (error "state->id: cannot find state to id mapping")))

; search st-id-map for the state that corresponds with the given id
(define (id->state id st-id-map)
  (let ([kvs (filter (λ (kv) (equal? id (cdr kv))) (hash->list st-id-map))])
    (if (null? kvs)
      (error "id->state: id not found in st-id-map")
      (car (car kvs)))))


; -------------------------------------
; Other helpers
; -------------------------------------
; display the state's store as html
(define (store->html store)
  (merge-strings
    (hash-map
      store
      (λ (k v)
         (format
           "<span style=\"color:blue;\">~a</span> \n~a\n\n"
           (pretty-format k) (pretty-format v))))))


(define (merge-strings lst)
  (foldl string-append "" lst))
