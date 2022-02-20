#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*              Dictionary (AVL-Tree Implementation)               *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (dictionary)
 (export new dictionary? insert! delete! find empty? full?)
 (import (rnrs base)
         (srfi :9)
         (prefix (a-d tree avl-tree) avl:))
 
 (define make-assoc cons)
 (define assoc-key car)
 (define assoc-value cdr)

 (define (lift proc)
   (lambda (assoc1 assoc2)
     (proc (assoc-key assoc1)
           (assoc-key assoc2))))
 
 (define (new ==? <<?)
   (avl:new 
    (lift ==?)
    (lift <<?)))
 
 (define (dictionary? any)
   (avl:bst? any))
 
 (define (insert! dct key val)
   (avl:insert! dct (make-assoc key val))
   dct)
 
 (define (delete! dct key)
   (avl:delete! dct (make-assoc key 'ignored))
   dct)
 
 (define (find dct key)
   (define assoc (avl:find dct (make-assoc key 'ignored)))
   (if assoc
     (assoc-value assoc)
     assoc))
 
 (define (empty? dct)
   (avl:empty? dct))
 
 (define (full? dct)
   (avl:full? dct)))