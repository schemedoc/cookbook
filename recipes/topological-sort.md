# Topological sort

## Problem

Implement a
[_topological sort_](https://en.wikipedia.org/wiki/Topological_sorting)
algorithm.

**Input:** A
[_directed graph_](https://en.wikipedia.org/wiki/Directed_graph)
describing how nodes depend on other nodes.

**Output:** An order in which to visit the nodes, such that the
dependencies of each node are visited before that node.

Raise an exception if the graph is cyclic.

A well-known application of topological sort is the Unix make program,
which uses it to compute the order in which to build files.

## Solution

The graph is represented as a list of lists. The first element of each
nested list is the label of a graph node, and the rest of the elements
(if any) are the labels of the nodes that this node depends on.

Depends on:

* The `filter` procedure from SRFI 1.
* The three-argument version of `assoc` from R7RS.

```Scheme
(define (topological-sort nodes eq)
  (define table (map (lambda (n) (cons (car n) 0)) nodes))
  (define queue '())
  (define result '())

  (define (set-up)
    ;; Compute the number of nodes that each node depends on.
    (for-each
      (lambda (node)
        (for-each
          (lambda (to)
            (let ((p (assoc to table eq)))
              (if p
                  (set-cdr! p (+ 1 (cdr p)))
                  (set! table (cons (cons to 1) table)))))
          (cdr node)))
      nodes))

  (define (traverse)
    (unless (null? queue)
      (let ((nq (car queue)))
        (set! queue (cdr queue))
        (let ((n0 (assoc nq nodes eq)))
          (when n0
            (for-each
              (lambda (to)
                (let ((p (assoc to table eq)))
                  (when p
                    (let ((cnt (- (cdr p) 1)))
                      (when (zero? cnt)
                        (set! result (cons to result))
                        (set! queue (cons to queue)))
                      (set-cdr! p cnt)))))
              (cdr n0)))
          (traverse)))))

  (set-up)
  (set! queue (map car (filter (lambda (p) (zero? (cdr p))) table)))
  (set! result queue)
  (traverse)
  (let ((rest (filter (lambda (e) (not (zero? (cdr e)))) table)))
    (unless (null? rest)
      (error "Graph has circular dependency" (map car rest))))
  (reverse result))
```

Credit: [Shiro Kawai](https://practical-scheme.net/)

## Built-in procedures

Chicken and Gauche have a built-in `topological-sort` procedure.

## Usage

### Valid graph

```Scheme
(topological-sort '((shirt tie belt)
                    (tie jacket)
                    (belt jacket)
                    (watch)
                    (pants shoes belt)
                    (undershorts pants shoes)
                    (socks shoes))
                  eqv?)
;; => (socks undershorts watch shirt tie pants belt jacket shoes)
```

### Cyclic graph

```Scheme
(topological-sort '((watch tie)
                    (tie watch))
                  eqv?)
;; Raises an exception.

(topological-sort '((shirt watch)
                    (watch tie)
                    (tie watch))
                  eqv?)
;; Raises an exception.
```
