#lang racket

#|
    TAREA 03: Programación Racket - Arbolitos
    Rubén Augusto Villalta López - 2017130955
    Ricardo Daniel Loaiza Rojas - 2017076031
|#

;Estructuras para el árbol
(struct node(id name value) #:mutable #:transparent)
(struct tree(nodeList edgeList) #:mutable #:transparent)

; Listar todos los nodos.
; Recibe una estructura de tipo árbol.
; Despliega el contenido completo de esta.
(define (list-all-nodes tree)
    (cond
    [(empty?(tree-nodeList tree)) "El árbol no tiene nodos"]
    [else(extract-nodes tree (node-id (first (tree-nodeList tree))))]))

; Buscar un nodo
; Recibe el árbol y el nodo a buscar.
; Devuelve todos los datos asociados a un nodo
(define (find-node tree id)
    (cond
    [(empty? tree) empty]
    [else(find-node-aux (tree-nodeList tree) id)]))

(define (find-node-aux alist id)
    (cond
    [(empty? alist) #f]
    [(eq? (node-id (first alist)) id) (first alist)]
    [else(find-node-aux (rest alist) id)]))

; Insertar un nuevo nodo en una posición determinada en el árbol.
; Recibe el árbol, el nodo padre y los datos del nodo a insertar.
; No retorna nada
(define (insert-node tree father-id id name value)
    (define temp(node id name value))
    (cond
    [(empty? (tree-nodeList tree)) (set-tree-nodeList! tree (append (tree-nodeList tree) (cons temp empty)))]
    [(check-id tree id) "Ya existe un nodo con ese ID"]
    [else(set-tree-edgeList! tree (append (tree-edgeList tree) (cons (cons father-id id) empty)))
         (set-tree-nodeList! tree (append (tree-nodeList tree) (cons temp empty)))]))

; Eliminar un nodo.
; Recibe el árbol y el nodo a eliminar.
; Retorna el nuevo árbol.
(define (delete-node tree id)
    (cond
    [(empty? (tree-nodeList tree)) "El árbol no tiene nodos"]
    [(check-id tree id)(delete-node-aux tree id (extract-children tree id))]
    [else "No existe el nodo"]))

(define (delete-node-aux tree id alist)
    (cond
    [(empty? alist) (delete-in-nodes tree id (tree-nodeList tree) empty)(delete-in-edges tree id (tree-edgeList tree) empty)]
    [else(delete-node-aux tree (node-id (first alist)) (extract-children tree (node-id (first alist)))) (delete-node-aux tree id (rest alist))]))

; Buscar el ancestro
; Recibe un árbol y el nodo para el cual se desea buscar el ancestro.
; Retorna el nodo padre
(define (ancestor tree id)
    (cond 
    [(eq? id (node-id (first (tree-nodeList tree)))) "Este es el nodo raíz"]
    [(check-id tree id) (ancestor-aux tree (tree-edgeList tree) id)]
    [else "No existe el nodo"]))

(define (ancestor-aux tree elist id)
    (cond
    [(empty? elist) empty]
    [(eq? (cdr (first elist )) id) (find-node tree (car (first elist)))]
    [else(ancestor-aux tree (rest elist) id)]))

;Funciones ayudantes

; Verifica si el ID esta en el arbol
; Recibe un arbol y un ID que se quiere verificar
; Retorna true o false
(define (check-id tree id)
    (cond
    [(empty? tree) #f]
    [else(check-id-aux (tree-nodeList tree) id)]))

(define (check-id-aux alist id)
    (cond
    [(empty? alist) #f]
    [(eq? (node-id (first alist)) id) #t]
    [else(check-id-aux (rest alist) id)]))

; Extrae todos los nodos ligados a un nodo
; Recibe un arbol y el ID del nodo que se quiere extraer
; Retorna una lista de nodos relacionados al ingresado
(define (extract-nodes tree id)
    (cond 
    [(check-id tree id) (cons (cons (find-node tree id) (extract-nodes-aux tree (tree-edgeList tree) id empty)) (cons id (extract-edges tree (tree-edgeList tree) id empty)))]
    [else "No existe el nodo"]))

(define (extract-nodes-aux tree alist id temp)
    (cond 
    [(empty? alist) temp]
    [(eq? (car (first alist)) id) (append temp (cons (find-node tree (cdr (first alist))) (cons (extract-nodes-aux tree (tree-edgeList tree)(node-id (find-node tree (cdr (first alist)))) temp)(extract-nodes-aux tree (rest alist) id temp))))]
    [else(extract-nodes-aux tree (rest alist) id temp)]))

; Extrae todas las aristas de los nodos relacionados a un nodo por el ID
; Recibe un arbol, una lista con todas las aristas, el id del nodo a buscar y una lista temporal
; Retorna la lista temporal con las aristas desde el punto del nodo ID
(define (extract-edges tree alist id temp)
    (cond 
    [(empty? alist) temp]
    [(eq? (car (first alist)) id) (append temp (cons (first alist) (cons (extract-edges tree (tree-edgeList tree)(cdr (first alist)) temp)(extract-edges tree (rest alist) id temp))))]
    [else(extract-edges tree (rest alist) id temp)]))

; Extrae los hijos de un nodo
; Recibe un arbol y el ID del nodo que se quiere obtener los hijos
; Retorna una lista con los hijos de un nodo
(define (extract-children tree id)
    (cond 
    [(empty? (tree-nodeList tree)) "No hay nodos en este arbol"]
    [else(extract-children-aux tree (tree-edgeList tree) id empty)]))

(define (extract-children-aux tree alist id temp)
    (cond 
    [(empty? alist) temp]
    [(eq? (car(first alist)) id) (append temp (cons (find-node tree (cdr (first alist)))(extract-children-aux tree (rest alist) id temp)))]
    [else(extract-children-aux tree (rest alist) id temp)]))

; Elimina el nodo de las lista de nodos
; Recibe el arbol, el ID del nodo que se quiere eliminar, la lista de nodos y una lista temporal.
; Retorna una lista sin el nodo que se quizo eliminar
(define (delete-in-nodes tree id alist temp)
    (cond 
    [(empty? alist) (set-tree-nodeList! tree temp)]
    [(eq? id (node-id (first alist))) (delete-in-nodes tree id (rest alist) temp)]
    [else(delete-in-nodes tree id (rest alist) (append temp (cons (first alist) empty)))]))

; Elimina las arista relacionadas al nodo de las lista de aristas
; Recibe el arbol, el ID del nodo que se quiere eliminar, la lista de nodos y una lista temporal.
; Retorna una lista sin las aritas del nodo que se quizo eliminar.
(define (delete-in-edges tree id alist temp)
    (cond 
    [(empty? alist) (set-tree-edgeList! tree temp)]
    [(eq? id (cdr (first alist))) (delete-in-edges tree id (rest alist) temp)]
    [else(delete-in-edges tree id (rest alist) (append temp (cons (first alist) empty)))]))