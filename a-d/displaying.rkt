#lang r6rs
(library
 (displaying-graphs)
 (export displayln display-first-phase display-graph display-louvain)
 (import
  (rnrs base)
  (rnrs io simple)
  (rnrs r5rs-6)
  (prefix (a-d graph weighted with-self-loops config) wg:)
  (prefix (a-d graph unweighted config) ug:)
  (prefix (a-d graph examples undirected-unweighted) ex:)
  (a-d louvain)
  (prefix (a-d tree binary-tree-algorithms) btalgo:)
  (prefix (a-d tree avl-tree) avl-tree:))

 
 (define (displayln obj)
   (display obj)
   (newline))
  

 (define (display-intra-degrees intra-degs)
   (displayln "community  intra-degree")
   (btalgo:iterative-pre-order (avl-tree:root intra-degs)
                               (lambda (el)
                                 (display (car el))
                                 (display "\t")
                                 (displayln (cdr el)))))
 
 (define (display-modularity-improvement mod-change)
   (display "Modularity improvement:\t")
   (displayln mod-change))

 (define (display-graph graph)
   (display "From\tto\tweight")
   (newline)
   (wg:for-each-node graph
                     (lambda (from)
                       (wg:for-each-edge graph
                                         from
                                         (lambda (weight to)
                                           (display from)
                                           (display "\t")
                                           (display to)
                                           (display "\t")
                                           (displayln weight))))))

 (define (display-communities communities)
   (display "communities:\t")
   (displayln communities))

 (define (display-pass idx)
   (display "pass\t")
   (displayln idx))

 (define (display-nr-of-nodes nr)
   (display "number of nodes:\t")
   (displayln nr))

 (define (display-name graph-name)
   (display graph-name)
   (displayln ":\t"))
 

 (define (display-first-phase results)
   (let ((modularity-improvement (exact->inexact (car results)))
         (communities (cadr results))
         (intra-degrees (caddr results)))
     (display-modularity-improvement modularity-improvement)
     (display-communities communities)
     (display-intra-degrees intra-degrees))
   (newline))

 (define (display-modularity graph communities)
   (let ((mod (modularity graph communities)))
     (display "Modularity:\t")
     (display mod)
     (display " = ")
     (displayln (exact->inexact mod))))
  

 (define (display-louvain results graph-name)
   (display-name graph-name)
   (let loop ((idx 0)
              ;; eerste pass is de voorlaatste conscel in de lijst
              (lst (reverse results)))
     (if (not (null? lst))
         (let ((communities (cdar lst))
               (graph (caar lst)))
           (display-pass idx)
           (display-modularity graph communities)
           (display-nr-of-nodes (vector-length communities))
           (display-communities communities)
           (display-graph graph)
           (newline)
           (loop (+ idx 1)
                 (cdr lst)))))))
