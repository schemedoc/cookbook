# Topological sort

## Problem

Implement the
[_topological sort_](https://en.wikipedia.org/wiki/Topological_sorting)
algorithm.

**Input:** A
[_directed acyclic graph_](https://en.wikipedia.org/wiki/Directed_acyclic_graph)
(DAG) describing how nodes depend on other nodes.

**Output:** An order in which to visit the nodes, such that the
dependencies of each node are visited before that node.

## Solution

```Scheme
(define (topological-sort nodes :optional (eq eqv?))
  (define table (map (^n (cons (car n) 0)) nodes))
  (define queue '())
  (define result '())

  ;; set up - compute number of nodes that each node depends on.
  (define (set-up)
    (dolist [node nodes]
      (dolist [to (cdr node)]
        (if-let1 p (assoc to table eq)
          (inc! (cdr p))
          (push! table (cons to 1))))))

  ;; traverse
  (define (traverse)
    (unless (null? queue)
      (let1 n0 (assoc (pop! queue) nodes eq)
        (when n0
          (dolist [to (cdr n0)]
            (if-let1 p (assoc to table eq)
              (let1 cnt (- (cdr p) 1)
                (when (= cnt 0)
                  (push! result to)
                  (push! queue to))
                (set! (cdr p) cnt)))))
        (traverse))))

  (set-up)
  (set! queue (append-map (^p (if (= (cdr p) 0) (list (car p)) '())) table))
  (set! result queue)
  (traverse)
  (let1 rest (filter (^e (not (zero? (cdr e)))) table)
    (unless (null? rest)
      (error "graph has circular dependency" (map car rest))))
  (reverse result))
```

Credit: [Shiro Kawai](https://practical-scheme.net/)

## Built-in procedures

Chicken and Gauche have a built-in `topological-sort` procedure.

## Usage

### Valid graph

```Scheme
(let ([sorted '(socks undershorts watch shirt tie pants belt jacket shoes)]
      [input '((shirt tie belt)
               (tie jacket)
               (belt jacket)
               (watch)
               (pants shoes belt)
               (undershorts pants shoes)
               (socks shoes))])

  (test* "topological-sort, default" sorted
         (topological-sort input))

  (test* "topological-sort, eq?" sorted
         (topological-sort input eq?))

  (test* "topological-sort, string=?"
         (map symbol->string sorted)
         (topological-sort (map (cut map symbol->string <>) input) string=?))
  )
```

### Cyclic graph

```Scheme
(topological-sort '((watch tie)
                    (tie watch)))
;; Raises an exception.

(topological-sort '((shirt watch)
                    (watch tie)
                    (tie watch)))
;; Raises an exception.
```
