#lang racket

(define (crear-tablero)
    (crear-tablero-aux '() 4 1 4))

(define (crear-tablero-aux tablero izq cen der)
    (cond
    [(>= (length tablero) 17) tablero]
    [(> (length tablero) 12) (cond
                           [(eq? izq der) (crear-tablero-aux (cons (append (llenar-lista izq 0) (append (llenar-lista cen 1) (llenar-lista der 0)))tablero) izq (- cen 1) (+ der 1))]
                           [else(crear-tablero-aux (cons (append (llenar-lista izq 0) (append (llenar-lista cen 1) (llenar-lista der 0)))tablero) (+ izq 1) (- cen 1) der)])]
    [(< (length tablero) 4) (cond
                          [(eq? izq der) (crear-tablero-aux (cons (append (llenar-lista izq 0) (append (llenar-lista cen 3) (llenar-lista der 0)))tablero) (- izq 1) (+ cen 1) der)]
                          [else(crear-tablero-aux (cons (append (llenar-lista izq 0) (append (llenar-lista cen 3) (llenar-lista der 0)))tablero) izq (+ cen 1) (- der 1))])]
    [else (cond ; centro 2 
          [(< (length tablero) 8) (cond
                                   [(eq? izq der) (crear-tablero-aux (cons (append (llenar-lista izq 0) (append (llenar-lista cen 2) (llenar-lista der 0)))tablero) (- izq 1) (+ cen 1) der)]
                                   [else(crear-tablero-aux (cons (append (llenar-lista izq 0) (append (llenar-lista cen 2) (llenar-lista der 0)))tablero) izq (+ cen 1) (- der 1))])]
          [else(cond
               [(eq? izq der) (crear-tablero-aux (cons (append (llenar-lista izq 0) (append (llenar-lista cen 2) (llenar-lista der 0)))tablero) izq (- cen 1) (+ der 1))]
               [else(crear-tablero-aux (cons (append (llenar-lista izq 0) (append (llenar-lista cen 2) (llenar-lista der 0)))tablero) (+ izq 1) (- cen 1) der)])])]))


(define (llenar-lista cantidad elemento)
    (cond
    [(<= cantidad 0) '()]
    [else (llenar-lista-aux cantidad elemento '())]))

(define (llenar-lista-aux cantidad elemento nueva)
    (cond
    [(eq? cantidad 0) nueva]
    [else (llenar-lista-aux (- cantidad 1) elemento (cons elemento nueva))]))

(define (mostrar-tablero tablero)
    (cond
    [(null? tablero) "Tablero Damas chinas"]
    [else (printf "~s\n" (first tablero)) (mostrar-tablero (rest tablero))]))


(define (verificar-movimiento tablero fila columna jugador); (fila ,columna) pertenecen a {0, length-1}, el index empieza con 0. Se debe restar las entradas si se usa el length
    (cond
    [(eq? (list-ref ( list-ref tablero fila) columna) 2) #t]
    [(eq? (list-ref (list-ref tablero fila) columna) (cambio-turno jugador)) #f]))

(define (cambio-turno jugador)
    (cond
    [(eq?  jugador 1) 3]
    [else 1]))

(define (cargar-fichas tablero)
    (cargar-fichas-aux tablero 0 0 '() '()))

(define (cargar-fichas-aux tablero fila columna fichas-cpu fichas-p)
    (cond
    [(and (> fila 15) (> columna 8)) (cons fichas-p (list fichas-cpu))] ;Caso de parada
    [(> columna 8) (cargar-fichas-aux tablero (+ fila 1) 0 fichas-cpu fichas-p)] ;Salta a la siguiente fila y reinicia la columna
    [(eq? (obtener-celda tablero fila columna) 1) (cargar-fichas-aux tablero fila (+ columna 1) (append (list (list fila columna)) fichas-cpu) fichas-p)]; Esto mete las fichas de la computadora en la primera lista de las fichas
    [(eq? (obtener-celda tablero fila columna) 3) (cargar-fichas-aux tablero fila (+ columna 1) fichas-cpu (append (list (list fila columna)) fichas-p))]; Esto mete las fichas de la computadora en la segunda lista de las fichas
    [else( cargar-fichas-aux tablero fila (+ columna 1) fichas-cpu fichas-p)]))

(define (obtener-celda tablero fila columna)
    (list-ref (list-ref tablero fila) columna))

(define (buscar-movimientos-vecinos tablero ficha anterior)
    (append (cond
            [(eq? (first ficha) 0) (list 
                                   (buscar-derecha tablero ficha anterior 0 empty)
                                   (buscar-inferior-derecho tablero ficha anterior 0 empty)
                                   (buscar-inferior-izquierdo tablero ficha anterior 0 empty)
                                   (buscar-izquierda tablero ficha anterior 0 empty))])
            (cond
            [(eq? (first ficha) 16) (list 
                                   (buscar-superior-izquierdo tablero ficha anterior 0 empty)
                                   (buscar-superior-derecho tablero ficha anterior 0 empty)
                                   (buscar-derecha tablero ficha anterior 0 empty)
                                   (buscar-izquierda tablero ficha anterior 0 empty))])
            (cond
            [(and (>= (first ficha) 7) (<= (first ficha) 9)) (cond 
                                                             [(eq? (second ficha) 0) (list
                                                                                     (buscar-superior-izquierdo tablero ficha anterior 0 empty)
                                                                                     (buscar-superior-derecho tablero ficha anterior 0 empty)
                                                                                     (buscar-derecha tablero ficha anterior 0 empty)
                                                                                     (buscar-inferior-derecho tablero ficha anterior 0 empty)
                                                                                     (buscar-inferior-izquierdo tablero ficha anterior 0 empty))]
                                                             [(eq? (second ficha) 8) (list
                                                                                     (buscar-superior-izquierdo tablero ficha anterior 0 empty)
                                                                                     (buscar-izquierdo tablero ficha anterior 0 empty)
                                                                                     (buscar-inferior-izquierdo tablero ficha anterior 0 empty))])]
            [else(list 
                 (buscar-superior-izquierdo tablero ficha anterior 0 empty)
                 (buscar-superior-derecho tablero ficha anterior 0 empty)
                 (buscar-derecha tablero ficha anterior 0 empty)
                 (buscar-inferior-derecho tablero ficha anterior 0 empty)
                 (buscar-inferior-izquierdo tablero ficha anterior 0 empty)
                 (buscar-izquierda tablero ficha anterior 0 empty))])))
(define (buscar-superior-izquierdo tablero ficha anterior contador movimientos); matriz, lista, num = 0, lista de coords
;incialmente anterior esta vacia
    (cond 
    [(or (eq? contador 2) 
         (eq? anterior ficha) 
         (eq? (obtener-celda tablero (sub1 (first ficha)) (second ficha)) 0)) 
      movimientos];Condición de parada, retorna la lista de los movimientos encadenads o una lista vacía
    [(and (eq? contador 1) 
          (eq? (obtener-celda tablero (sub1 (first ficha)) (second ficha)) 2)) 
    (buscar-movimientos-vecinos tablero (list (sub1 (first ficha)) (second ficha)) ficha)];Verifica si ya ha hecho un salto además verifica si la proxima casilla hay posibilidad de saltar sobre ella y buscar combos de saltos.
    [(not (eq? (obtener-celda tablero (sub1 (first ficha)) (second ficha)) 2)) 
    (buscar-superior-izquierdo tablero (list (sub1 (first ficha)) (second ficha)) ficha (add1 contador) movimientos)];Verifica si hay un 1 o 3 si es así lo salta sino no
    [else(buscar-movimientos-vecinos tablero (list (sub1 (first ficha)) (second ficha)) ficha)]
(define (miembro? lista elemento)
  (cond
    [(not (eq? (member elemento lista) #f)) #t]
    [else #f]))


(mostrar-tablero (crear-tablero))
;(obtener-celda (crear-tablero) 15 4)

; (cargar-fichas (crear-tablero))
; (cargar-movimientos (crear-tablero) (cargar-fichas (crear-tablero)))

; (obtener-celda (cargar-fichas (crear-tablero)) 0 0)
; (cargar-fichas (crear-tablero)) 
(mover-superior-izquierdo 1 2 3 4)