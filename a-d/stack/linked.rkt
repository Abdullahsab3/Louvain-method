#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                 Stacks (Linked Implementation)                  *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                  2011  Software Languages Lab                   *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (stack)
 (export new stack? push! pop! top empty? full?)
 (import (rnrs base)
         (srfi :9)
         (rnrs mutable-pairs))
 
 (define-record-type stack
   (make l)
   stack?
   (l scheme-list scheme-list!))
 
 (define (new)
   (make ()))
 
 (define (push! stack val)
   (define slst (scheme-list stack))
   (scheme-list! stack (cons val slst))
   stack)
 
 (define (top stack)
   (define slst (scheme-list stack))
   (if (null? slst)
     (error "stack empty (top)" stack))
   (car slst))
 
 (define (pop! stack)
   (define slst (scheme-list stack))
   (if (null? slst)
     (error "stack empty (pop!)" stack))
   (let ((val (car slst)))
     (scheme-list! stack (cdr slst))
     val))
 
 (define (empty? stack)
   (define slst (scheme-list stack))
   (null? slst))
 
 (define (full? stack)
   #f))