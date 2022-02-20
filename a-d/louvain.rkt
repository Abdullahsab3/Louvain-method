#lang r6rs
(library
 (louvain-method)
 (export louvain detect-communities aggregate degrees modularity in-community-degree) ; DO NOT CHANGE THIS LINE
 (import (rnrs base)
         (rnrs mutable-pairs-6)
         (prefix (a-d graph weighted with-self-loops config) g:)
         (prefix (a-d dictionary ordered avl) avl:)
         (prefix (a-d tree avl-tree) avl-tree:)
         (prefix (a-d tree binary-tree-algorithms) btalgo:))
         

 ;;;; MANDATORY Auxiliary procedures 

 ; degrees (weighted-graph -> vector<number>)
 ; Returns a node-indexed vector containing the degree of each node of a weighted graph
 (define (degrees graph)
   (define vector-out (make-vector (g:order graph)))
   (g:for-each-node graph (lambda (from)
                            (g:for-each-edge
                             graph
                             from
                             (lambda (weight to)
                               (vector-set! vector-out from
                                            (+ (vector-ref vector-out from) 1))))))
   vector-out)
 

 ; modularity (weighted-graph vector<community-id> -> number)
 ; General formula to calculate modularity in a weighted graph given its communities

 ;; abstracties
 (define (get-degree degrees node)
   (vector-ref degrees node))
 
 (define (get-community communities node)
   (vector-ref communities node))
 
 (define (same-community? ci cj)
   (eq? ci cj))

 ;; het totale gewicht van een graaf berekenen (in de formules: W)
 (define (total-weight graph)
   (let ((tot-weight 0))
     (g:for-each-node graph
                      (lambda (from)
                        (g:for-each-edge graph from
                                         (lambda (weight to)
                                           (set! tot-weight
                                                 (+ tot-weight weight))))))
     tot-weight))

 (define (modularity graph communities)
 
   (define (delta ci cj)
     (if (same-community? ci cj)
         1
         0))
   
   (let ((W (total-weight graph))
         (degs (degrees graph))
         (sum 0))

     ;; W mag niet 0 zijn. we delen door W 
     (if (zero? W)
         (error "Modularity: The total weight of the given graph is zero"))
     
     (g:for-each-node graph
                      (lambda (from)
                        (g:for-each-node graph 
                                         (lambda (to)
                                           (let* ((ci (get-community communities from))
                                                  (cj (get-community communities to))
                                                  (delta-value (delta ci cj)))
                                             ;; als delta 0 is, dan zal de term toch 0 zijn
                                             (if (= delta-value 1)
                                                 (let ((weight
                                                        (let ((pot-weight (g:weight graph from to)))
                                                          ;; als het gewicht gelijk is aan +inf.0, dan is er geen boog tussen die nodes
                                                          (if (= pot-weight +inf.0)
                                                              0
                                                              pot-weight)))
                                                       (ki (get-degree degs from))
                                                       (kj (get-degree degs to)))
                                         
                                                   (set! sum (+ sum (- weight (/ (* ki kj) W)))))))))))
     (/ sum W)))


 ; in-community-degree (weighted-graph vector<community-id> number community-id -> number)
 ; Procedure to calculate the sum of weights of edges from a specific node leading to other nodes in a specific community.
 ; Note: This sum has to be doubled (* 2) because we work with undirected graphs.
 (define (self-loop? from to)
   (= from to))
 
 (define (in-community-degree graph communities node comm-id)
   (let ((sum 0))
     
     (g:for-each-edge graph node
                      (lambda (weight to)
                        (if (same-community? (get-community communities to) comm-id)
                            ;; als het een zelfboog is, dan moeten we het gewicht niet verdubbelen
                            (set! sum (+ sum (if (self-loop? node to)
                                                 weight
                                                 (* 2 weight)))))))
     sum))

 ;;;; PHASE 1: Modularity Optimization

 ; detect-communities (weighted-graph -> {number, vector<community-id>, dictionary<community-id, number>} )
 ; Assigns nodes of a weighted graph to a community by optimizing modularity.
 ; Returns a list containing:
 ;     (1) the amount of improvement in modularity that was made;
 ;     (2) a node-indexed vector of community id's;
 ;     (3) a dictionary mapping communities to their internal degree

 (define (sum-all-degs-in-community graph degs communities comm)
   (let ((sum 0))
     (g:for-each-node graph
                      (lambda (node)
                        (if (same-community? (get-community communities node) comm)
                            (set! sum (+ sum (get-degree degs node))))))
     sum))
   
 ;; delta Q
 ;; W en degs worden meegegeven en niet in de procedure berekend omdat ze niet veranderen voor dezelfde graaf
 ;; en hoeven dus niet telkens opnieuw berekend te worden
 (define (modularity-change graph W degs communities comm-A comm-B node-i)
   (if (zero? W)
       (error "Modularity-change: The total weight of the given graph is zero")
       (let* ((kiA (in-community-degree graph communities node-i comm-A))
              (kiB (in-community-degree graph communities node-i comm-B))
              (ki (get-degree degs node-i))
              (degs-sum-A (sum-all-degs-in-community graph degs communities comm-A))
              (degs-sum-B (sum-all-degs-in-community graph degs communities comm-B)))
           
         (/ (+ (- kiB kiA)
               (/ (* 2
                     ki
                     (- degs-sum-A ki degs-sum-B))
                  W))
            W))))
  

 ;; initieel zit iedere node in zijn eigen community
 (define (initialize-community-vector comm-ids)
   (define vec-length (vector-length comm-ids))
   (let loop ((idx 0))
     (if (< idx vec-length)
         (begin (vector-set! comm-ids idx idx)
                (loop (+ idx 1))))))



 ;; een procedure om een dictionary van de de intra-degrees te maken
 (define (get-intra-degrees graph communities degs)
   ;; er wordt gebruikgemaakt van een avl.
   ;; efficient om erin te zoeken
   (let ((intra-degrees (avl:new = <)))
     (g:for-each-node graph (lambda (from)
                              (if (zero? (get-degree degs from))
                                  ;; als de graad van de node 0 is, dan is het een geisoleerde node die in eigen community zit
                                  ;; in dat geval inserten we deze node aan de dictionary met intra-degree 0
                                  ;; omdat er geen bogen zijn.
                                  ;; een van de redenen waarom we een geisoleerde community inserten is
                                  ;; omdat we het aantal communities berekenen adhv de dictionary.
                                  (avl:insert! intra-degrees (get-community communities from) 0)
                                  (g:for-each-edge graph
                                                   from
                                                   (lambda (weight to)
                                                     (let ((comm-from (get-community communities from))
                                                           (comm-to (get-community communities to)))
                                                       (if (same-community? comm-from comm-to)
                                                           (avl:insert! intra-degrees
                                                                        comm-from
                                                                        ;; als de intra-degree van deze community al erin zit
                                                                        ;; dan gaan we het updaten en verhogen met het gewicht van de curr boog
                                                                        (let ((old-intra-degree (avl:find intra-degrees comm-from)))
                                                                          (if old-intra-degree
                                                                              (+ weight old-intra-degree)
                                                                              weight))))))))))
     intra-degrees))
 

 ;; abstracties (in de heap steken we een conscel met de community in de car en de modularity change in de cdr)
 (define get-max-community car)
 (define get-modularity-change cdr)
 (define set-max-community! set-car!)
 (define set-max-modularity-change! set-cdr!)
 

 
 (define (detect-communities graph)
   (let ((comm-ids (make-vector (g:order graph)))
         (degs (degrees graph))
         (W (total-weight graph))
         ;; de totale verbetering van de modulariteit
         (modularity-improvement 0))
     
     (initialize-community-vector comm-ids)

     ;; het werkpaard van de eerste fase
     (define (iterate-over-adjacent-nodes from)
       (let ((max-modularity-change (cons 'unspecified 0)))
             
         (g:for-each-edge graph
                          from
                          (lambda (weight to)
                            (let ((current-modularity-change (modularity-change graph
                                                                                W
                                                                                degs
                                                                                comm-ids
                                                                                (get-community comm-ids from)
                                                                                (get-community comm-ids to)
                                                                                from)))
                              ;; we zoeken naar de grooteste positieve modularity change bij verschuiving van de huidige node
                              ;; naar een adjacent community
                              (if (> current-modularity-change
                                     (get-modularity-change max-modularity-change))
                                  (begin (set-max-community! max-modularity-change (get-community comm-ids to))
                                         (set-max-modularity-change! max-modularity-change current-modularity-change))))))
         
         (let ((mod (get-modularity-change max-modularity-change))
               (comm (get-max-community max-modularity-change)))
                                
           (if (> mod 0)
               (begin (vector-set! comm-ids from comm)
                      (set! modularity-improvement (+ modularity-improvement mod)))))))

     (let loop ((prev-improvement modularity-improvement))
       (g:for-each-node graph iterate-over-adjacent-nodes)
       ;; we blijven itereren over de nodes zolang er improvement mogelijk is
       (if (> modularity-improvement prev-improvement)
           (loop modularity-improvement)))
     (list modularity-improvement comm-ids (get-intra-degrees graph comm-ids degs))))


 ;;;; PHASE 2: Community Aggregation
 
 ; aggregate (weighted-graph vector<community-id> dictionary<community-id, number> -> weighted-graph)
 ; Generates the aggregated weighted graph given:
 ;     (1) a weighted graph;
 ;     (2) a node-indexed vector of its communities;
 ;     (3) a dictionary mapping communities to their intra-degree



 ;; berekenen hoeveel communities er zijn
 (define (how-many-communities sigma-ins)
   
   (let ((number-of-communities 0))
     ;; dit wordt berekend door de avl boom die de  dictionary<community-id, number> voorstelt te traversen vanuit de root
     (btalgo:iterative-pre-order (avl-tree:root sigma-ins)
                                 (lambda (el)
                                   (set! number-of-communities (+ number-of-communities 1))))
     number-of-communities))

 ;; een dictionary met de community als key en de nieuwe node in de geaggregeerde graaf als value
 ;; en een vector met de nodes van de geaggregeerde graaf als keys (indices) en de communities als values (inhoud van de "vectorkottekes")
 ;; beiden worden gegenereerd om efficient te zoeken:
 ;; wanneer het nodig is om te weten welke community zit in een bepaalde node in de nieuwe graaf, dan wordt de vector gebruikt, en is het zoeken O(1)
 ;; wanneer het nodig is om te weten in welke node een bepaalde community zit, wordt de avl dictionary gebruike, dan is het zoeken O(log(n))
 
 (define (sigma-ins-to-node-avl-&-node-indexed-vector sigma-ins)
   (let* ((node-avl (avl:new = <))
          (node-indexed-vector (make-vector (how-many-communities sigma-ins)))
          (idx 0))
     
     (btalgo:iterative-pre-order (avl-tree:root sigma-ins)
                                 (lambda (el)
                                   (avl:insert! node-avl (car el) idx)
                                   (vector-set! node-indexed-vector idx el)
                                   (set! idx (+ idx 1))))
     
     (cons node-indexed-vector node-avl)))

 (define get-community-weight cdr)
 
 (define (aggregate graph communities sigma-ins)
   (let* ((avl-&-vector (sigma-ins-to-node-avl-&-node-indexed-vector sigma-ins))
          (node-indexed-vector (car avl-&-vector))
          (node-avl (cdr avl-&-vector))
          (number-of-communities (vector-length node-indexed-vector))
          ;; de nieuwe graaf
          (aggregated-graph (g:new #f number-of-communities)))

     ;; de zelfbogen bij elke node (met het totale gewicht van de corresponderende community als gewicht)
     (g:for-each-node aggregated-graph
                      (lambda (node)
                        (g:add-edge! aggregated-graph
                                     node
                                     node
                                     (get-community-weight (vector-ref node-indexed-vector node)))))

     ;; de bogen tussen de nodes genereren
     (g:for-each-node graph
                      (lambda (from)
                        (g:for-each-edge graph
                                         from
                                         (lambda (raw-weight to)
                                           (define edge-weight (if (self-loop? from to)
                                                                   raw-weight
                                                                   (/ raw-weight 2)))
                                       
                                           (let ((community-from (get-community communities from))
                                                 (community-to  (get-community communities to)))

                                             ;; als beide nodes niet in dezelfde community zitten,
                                             ;; dan is die boog een boog uit een community naar een andere community
                                             (if (not (same-community? community-from community-to))
                                                 (let* ((community-from-node-in-aggregated-graph (avl:find node-avl community-from))
                                                        (community-to-node-in-aggregated-graph (avl:find node-avl community-to))
                                                        (aggregated-edge-weight (g:weight aggregated-graph
                                                                                          community-from-node-in-aggregated-graph
                                                                                          community-to-node-in-aggregated-graph)))
                                                       

                                                   ;; als er al een boog is tussen de corresponderende nodes in de geaggregeerde graaf,
                                                   ;; dan wordt zijn gewicht verhoogd
                                                   (if (= aggregated-edge-weight +inf.0)
                                                       (g:add-edge! aggregated-graph
                                                                    community-from-node-in-aggregated-graph
                                                                    community-to-node-in-aggregated-graph
                                                                    edge-weight)
                                                       
                                                       (begin (g:delete-edge! aggregated-graph
                                                                              community-from-node-in-aggregated-graph
                                                                              community-to-node-in-aggregated-graph)
                                                     
                                                              (g:add-edge! aggregated-graph
                                                                           community-from-node-in-aggregated-graph
                                                                           community-to-node-in-aggregated-graph
                                                                           (+ aggregated-edge-weight edge-weight)))))))))))
     aggregated-graph))

 
 ;;;; The top-level structure of the Louvain Method

 ; DO NOT CHANGE THIS PROCEDURE
 (define (louvain graph)
   (let iter ((hierarchy '())
              (current-graph graph))
     (let* ((first-phase (detect-communities current-graph))
            (improvement (car first-phase))
            (communities (cadr first-phase))
            (sigma-ins (caddr first-phase)))
       (if (> improvement 0) ; As long as we can improve modularity, we continue
           (iter (cons (cons current-graph communities)
                       hierarchy)
                 (aggregate current-graph communities sigma-ins))
           (cons (cons current-graph communities)
                 hierarchy)))))
 )