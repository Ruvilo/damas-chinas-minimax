#lang racket
;Funciones que creadoras
(define (crear-tablero )
    (crear-tablero-aux empty 4))

(define (crear-tablero-aux tablero fichas)
    (cond
    [(>= (length tablero) 9) tablero]
    [(< (length tablero) 4) (crear-tablero-aux (cons (append (llenar-lista fichas 1) (llenar-lista (- 9 fichas) 2)) tablero) (sub1 fichas))]
    [else(crear-tablero-aux (cons (append (llenar-lista (- 9 fichas) 2) (llenar-lista fichas 3)) tablero) (add1 fichas))]))   

(define (llenar-lista cantidad elemento)
    (cond
    [(<= cantidad 0) '()]
    [else (llenar-lista-aux cantidad elemento '())]))

(define (llenar-lista-aux cantidad elemento nueva)
    (cond
    [(eq? cantidad 0) nueva]
    [else (llenar-lista-aux (- cantidad 1) elemento (cons elemento nueva))]))

(define (cargar-fichas tablero)
    (cargar-fichas-aux tablero 0 0 '() '()))

(define (cargar-fichas-aux tablero fila columna cpu player)
    (cond
    [(> fila 8) (cons player (list cpu))]
    [(> columna 8) (cargar-fichas-aux tablero (add1 fila) 0 cpu player)]
    [(eq? (obtener-celda tablero fila columna) 1) (cargar-fichas-aux tablero fila (+ columna 1) (append (list (list fila columna)) cpu) player)]; Esto mete las fichas de la computadora en la primera lista de las fichas
    [(eq? (obtener-celda tablero fila columna) 3) (cargar-fichas-aux tablero fila (+ columna 1) cpu (append (list (list fila columna)) player))]; Esto mete las fichas de la computadora en la segunda lista de las fichas
    [else( cargar-fichas-aux tablero fila (+ columna 1) cpu player)]))

(define (obtener-celda tablero fila columna)
    (cond
    [(or 
        (< fila 0)
        (< columna 0)
        (> fila (sub1 (length tablero)))
        (> columna (sub1 (length tablero)))) 0]
    [else(list-ref (list-ref tablero fila) columna)]))

;Funciones de movimientos
(define (miembro? lista elemento)
  (cond
    [(not (eq? (member elemento lista) #f)) #t]
    [else #f]))


(define (buscar-movimientos-vecinos tablero ficha anteriores movimientos);return lista con los campos que se pueden mover la ficha que se mete
    (append  (cond
            [(and (eq? (first ficha) 0) (eq? (second ficha) 0))
            (append 
                   (buscar-inferior-derecho tablero ficha anteriores 0 movimientos)          ;ID
                   (buscar-inferior-izquierdo tablero ficha anteriores 0 movimientos))]      ;II
            [(and (eq? (first ficha) 0) (eq? (second ficha) 8))
            (append 
                   (buscar-superior-izquierdo tablero ficha anteriores 0 movimientos)        ;si
                   (buscar-izquierdo tablero ficha anteriores 0 movimientos)                 ;i
                   (buscar-inferior-izquierdo tablero ficha anteriores 0 movimientos)        ;II
            )]    
            [(and (eq? (first ficha) 8) (eq? (second ficha) 0))
            (append 
                   (buscar-superior-derecho tablero ficha anteriores 0 movimientos)          ;SD
                   (buscar-derecho tablero ficha anteriores 0 movimientos)                   ;D
                   (buscar-inferior-derecho tablero ficha anteriores 0 movimientos)          ;ID
            )]
            [(and (eq? (first ficha) 8) (eq? (second ficha) 8))
            (append
                   (buscar-superior-derecho tablero ficha anteriores 0 movimientos)          ;SD
                   (buscar-superior-izquierdo tablero ficha anteriores 0 movimientos)        ;SI
            )]
            [(eq? (first ficha) 0)
            (append
                   (buscar-superior-izquierdo tablero ficha anteriores 0 movimientos) ;SI
                   (buscar-inferior-derecho tablero ficha anteriores 0 movimientos) ;ID
                   (buscar-izquierdo tablero ficha anteriores 0 movimientos) ;I
                   (buscar-inferior-izquierdo tablero ficha anteriores 0 movimientos) ;II
            )]
            [(eq? (first ficha) 8)
            (append
                   (buscar-superior-derecho tablero ficha anteriores 0 movimientos) ;SD
                   (buscar-derecho tablero ficha anteriores 0 movimientos) ;D
                   (buscar-superior-izquierdo tablero ficha anteriores 0 movimientos) ;SI
                   (buscar-inferior-derecho tablero ficha anteriores 0 movimientos) ;ID
            )]
            [(eq? (second ficha) 0)
            (append
                   (buscar-superior-derecho tablero ficha anteriores 0 movimientos) ;SI
                   (buscar-inferior-derecho tablero ficha anteriores 0 movimientos) ;ID
                   (buscar-derecho tablero ficha anteriores 0 movimientos) ;I
                   (buscar-inferior-izquierdo tablero ficha anteriores 0 movimientos) ;II
            )]
            [(eq? (second ficha) 8)
            (append 
                  (buscar-superior-derecho tablero ficha anteriores 0 movimientos) ;SD
                  (buscar-inferior-izquierdo tablero ficha anteriores 0 movimientos)
                  (buscar-superior-izquierdo tablero ficha anteriores 0 movimientos)
                  (buscar-izquierdo tablero ficha anteriores 0 movimientos)
            )]
            [else(append
                    (buscar-superior-izquierdo tablero ficha anteriores 0 movimientos) ;SI retornar una lista con los movimientos vecinos superior izquierda
                    (buscar-superior-derecho tablero ficha anteriores 0 movimientos) ;SD retornar una lista con los movimientos vecinos superior izquierda
                    (buscar-derecho tablero ficha anteriores 0 movimientos) ;D
                    (buscar-inferior-derecho tablero ficha anteriores 0 movimientos) ;ID
                    (buscar-inferior-izquierdo tablero ficha anteriores 0 movimientos);II
                    (buscar-izquierdo tablero ficha anteriores 0 movimientos))];I
    ) movimientos))

(define (buscar-superior-derecho tablero ficha anteriores contador movimientos); SD -1,0
   (cond 
    [(or
        (eq? contador 3)
        (miembro? anteriores ficha)
        (eq? (obtener-celda tablero (sub1 (first ficha)) (second ficha)) 0))
         movimientos]
    [(and 
        (eq? contador 1)
        (eq? (obtener-celda tablero (first ficha) (second ficha)) 2))
            (buscar-superior-derecho tablero (list (sub1 (first ficha)) (second ficha)) (cons ficha anteriores) 3
                (append (buscar-movimientos-vecinos tablero ficha (cons ficha anteriores) (append (list ficha)  movimientos)) movimientos))]
    [(and
        (eq? (obtener-celda tablero (first ficha) (second ficha)) 2)
        (eq? (obtener-celda tablero (sub1 (first ficha)) (second ficha)) 2))
            (buscar-superior-derecho tablero ficha (cons ficha (cons (list (sub1 (first ficha)) (second ficha)) anteriores)) 3 
                (append (buscar-movimientos-vecinos tablero ficha (cons ficha (cons (list (sub1 (first ficha)) (second ficha)) anteriores)) (append (list ficha) movimientos)) movimientos))]
    [(eq? (obtener-celda tablero (first ficha) (second ficha)) 2)
        (buscar-superior-derecho tablero ficha (cons ficha anteriores) (add1 contador) 
            (append (buscar-movimientos-vecinos tablero ficha (cons ficha anteriores) (append (list ficha)  movimientos)) movimientos))]
    [(not (eq? (obtener-celda tablero (first ficha) (second ficha)) 2))
        (cond
        [(eq? (obtener-celda tablero(sub1 (first ficha)) (second ficha)) 2) 
            (buscar-superior-derecho tablero (list (sub1 (first ficha)) (second ficha)) (cons ficha anteriores) (add1 contador) 
                (append (buscar-movimientos-vecinos tablero  (list (sub1 (first ficha)) (second ficha)) (cons ficha anteriores) (append (list ficha) movimientos)) movimientos))]
        [else(buscar-superior-derecho tablero (list (sub1 (first ficha)) (second ficha)) (cons ficha anteriores) (add1 contador) movimientos)]) ]
    [else(buscar-superior-derecho tablero (list (sub1 (first ficha)) (second ficha)) (cons ficha anteriores) (add1 contador) 
    (append (buscar-movimientos-vecinos tablero (list (sub1 (first ficha)) (second ficha)) (cons ficha (cons (list (sub1 (first ficha)) (second ficha)) anteriores)) movimientos)  movimientos))]))

(define (buscar-superior-izquierdo tablero ficha anteriores contador movimientos) ;SI 0,-1
    (cond 
    [(or
        (eq? contador 3)
        (miembro? anteriores ficha)
        (eq? (obtener-celda tablero (first ficha) (sub1 (second ficha))) 0))
    movimientos]; condicion de parada
    [(and 
        (eq? contador 1)
        (eq? (obtener-celda tablero (first ficha) (second ficha)) 2))
    (buscar-superior-izquierdo tablero (list (first ficha) (sub1 (second ficha))) (cons ficha anteriores) 3
        (append (buscar-movimientos-vecinos tablero ficha (cons ficha anteriores) (append (list ficha) movimientos)) movimientos))]
    [(and
        (eq? (obtener-celda tablero (first ficha) (second ficha)) 2)
        (eq? (obtener-celda tablero (first ficha) (sub1 (second ficha))) 2))
    (buscar-superior-izquierdo tablero ficha (cons ficha (cons (list (first ficha) (sub1 (second ficha))) anteriores)) 3 
    (append (buscar-movimientos-vecinos tablero ficha (cons ficha (cons (list (first ficha) (sub1 (second ficha))) anteriores)) (append (list ficha)  movimientos)) movimientos))]
    [(eq? (obtener-celda tablero (first ficha) (second ficha)) 2)
    (buscar-superior-izquierdo tablero ficha (cons ficha anteriores) 3 
        (append (buscar-movimientos-vecinos tablero ficha (cons ficha anteriores) (append (list ficha)  movimientos)) movimientos))]
    [(not (eq? (obtener-celda tablero (first ficha) (second ficha)) 2))
        (cond
        [(eq? (obtener-celda tablero (first ficha) (sub1 (second ficha))) 2) 
            (buscar-superior-izquierdo tablero (list (first ficha) (sub1 (second ficha))) (cons ficha anteriores) (add1 contador) 
            (append (buscar-movimientos-vecinos tablero  (list (first ficha) (sub1 (second ficha))) (cons ficha anteriores)(append (list ficha) movimientos)) movimientos))]
        [else(buscar-superior-izquierdo tablero (list (first ficha) (sub1 (second ficha))) (cons ficha anteriores) (add1 contador) movimientos)]) ]
    [else(buscar-superior-izquierdo tablero  (list (first ficha) (sub1 (second ficha))) (cons ficha anteriores) (add1 contador) 
    (append (buscar-movimientos-vecinos tablero  (list (first ficha) (sub1 (second ficha))) (cons ficha (cons (list (first ficha) (sub1 (second ficha))) anteriores)) movimientos)  movimientos))]))


(define (buscar-derecho tablero ficha anteriores contador movimientos) ; D -1,+1
    (cond 
    [(or
        (eq? contador 3)
        (miembro? anteriores ficha)
        (eq? (obtener-celda tablero (sub1 (first ficha)) (add1 (second ficha))) 0))
         movimientos]
    [(and 
        (eq? contador 1)
        (eq? (obtener-celda tablero (first ficha) (second ficha)) 2))
    (buscar-derecho tablero (list (sub1 (first ficha)) (add1 (second ficha))) (cons ficha anteriores) 3
        (append (buscar-movimientos-vecinos tablero ficha (cons ficha anteriores) (append (list ficha)  movimientos)) movimientos))]
    [(and
        (eq? (obtener-celda tablero (first ficha) (second ficha)) 2)
        (eq? (obtener-celda tablero (sub1 (first ficha)) (add1 (second ficha))) 2))
    (buscar-derecho tablero ficha (cons ficha (cons (list (sub1 (first ficha)) (add1 (second ficha))) anteriores)) 3 
    (append (buscar-movimientos-vecinos tablero ficha (cons ficha (cons (list (sub1 (first ficha)) (add1 (second ficha))) anteriores)) (append (list ficha)  movimientos)) movimientos))]
    [(eq? (obtener-celda tablero (first ficha) (second ficha)) 2)
    (buscar-derecho tablero ficha (cons ficha anteriores) 3 
        (append (buscar-movimientos-vecinos tablero ficha (cons ficha anteriores) (append (list ficha)  movimientos)) movimientos))]
    [(not (eq? (obtener-celda tablero (first ficha) (second ficha)) 2))
        (cond
        [(eq? (obtener-celda tablero (sub1 (first ficha)) (add1 (second ficha))) 2) 
            (buscar-derecho tablero (list (sub1 (first ficha)) (add1 (second ficha))) (cons ficha anteriores) (add1 contador) 
            (append (buscar-movimientos-vecinos tablero  (list (sub1 (first ficha)) (add1 (second ficha))) (cons ficha anteriores)(append (list ficha) movimientos)) movimientos))]
        [else(buscar-derecho tablero (list (sub1 (first ficha)) (add1 (second ficha))) (cons ficha anteriores) (add1 contador) movimientos)]) ]
    [else(buscar-derecho tablero (list (sub1 (first ficha)) (add1 (second ficha))) (cons ficha anteriores) (add1 contador) 
    (append (buscar-movimientos-vecinos tablero (list (sub1 (first ficha)) (add1 (second ficha))) (cons ficha (cons (list (sub1 (first ficha)) (add1 (second ficha))) anteriores)) movimientos) movimientos))]))


(define (buscar-izquierdo tablero ficha anteriores contador movimientos); I +1,-1
    (cond 
    [(or
        (eq? contador 3)
        (miembro? anteriores ficha)
        (eq? (obtener-celda tablero (add1 (first ficha)) (sub1 (second ficha))) 0))
         movimientos]
    [(and 
        (eq? contador 1)
        (eq? (obtener-celda tablero (first ficha) (second ficha)) 2))
    (buscar-izquierdo tablero (list (add1 (first ficha)) (sub1 (second ficha))) (cons ficha anteriores) 3
        (append (buscar-movimientos-vecinos tablero ficha (cons ficha anteriores) (append (list ficha) movimientos)) movimientos))]
    [(and
        (eq? (obtener-celda tablero (first ficha) (second ficha)) 2)
        (eq? (obtener-celda tablero (add1 (first ficha)) (sub1 (second ficha))) 2))
    (buscar-izquierdo tablero ficha (cons ficha (cons (list (add1 (first ficha)) (sub1 (second ficha))) anteriores)) 3 
    (append (buscar-movimientos-vecinos tablero ficha (cons ficha (cons (list (add1 (first ficha)) (sub1 (second ficha))) anteriores)) (append (list ficha) movimientos)) movimientos))]
    [(eq? (obtener-celda tablero (first ficha) (second ficha)) 2)
    (buscar-izquierdo tablero ficha (cons ficha anteriores) 3 
        (append (buscar-movimientos-vecinos tablero ficha (cons ficha anteriores) (append (list ficha) movimientos)) movimientos))]
    [(not (eq? (obtener-celda tablero (first ficha) (second ficha)) 2))
        (cond
        [(eq? (obtener-celda tablero (add1 (first ficha)) (sub1 (second ficha))) 2) 
            (buscar-izquierdo tablero (list (add1 (first ficha)) (sub1 (second ficha))) (cons ficha anteriores) (add1 contador) 
            (append (buscar-movimientos-vecinos tablero  (list (add1 (first ficha)) (sub1 (second ficha))) (cons ficha anteriores) (append (list ficha) movimientos)) movimientos))]
        [else(buscar-izquierdo tablero (list (add1 (first ficha)) (sub1 (second ficha))) (cons ficha anteriores) (add1 contador) movimientos)]) ]
    [else(buscar-izquierdo tablero (list (add1 (first ficha)) (sub1 (second ficha))) (cons ficha anteriores) (add1 contador) 
    (append (buscar-movimientos-vecinos tablero (list (add1 (first ficha)) (sub1 (second ficha))) (cons ficha (cons (list (add1 (first ficha)) (sub1 (second ficha))) anteriores)) movimientos)  movimientos))]))


(define (buscar-inferior-derecho tablero ficha anteriores contador movimientos) ; ID 0,+1
    (cond 
    [(or
        (eq? contador 3)
        (miembro? anteriores ficha)
        (eq? (obtener-celda tablero (first ficha) (add1 (second ficha))) 0))
         movimientos]
    [(and 
        (eq? contador 1)
        (eq? (obtener-celda tablero (first ficha) (second ficha)) 2))
    (buscar-inferior-derecho tablero (list (first ficha) (add1 (second ficha))) (cons ficha anteriores) 3
        (append (buscar-movimientos-vecinos tablero ficha (cons ficha anteriores) (append (list ficha) movimientos)) movimientos))]
    [(and
        (eq? (obtener-celda tablero (first ficha) (second ficha)) 2)
        (eq? (obtener-celda tablero (first ficha) (add1 (second ficha))) 2))
    (buscar-inferior-derecho tablero ficha (cons ficha (cons (list (first ficha) (add1 (second ficha))) anteriores)) 3 
    (append (buscar-movimientos-vecinos tablero ficha (cons ficha (cons (list (first ficha) (add1 (second ficha))) anteriores)) (append (list ficha) movimientos)) movimientos))]
    [(eq? (obtener-celda tablero (first ficha) (second ficha)) 2)
    (buscar-inferior-derecho tablero ficha (cons ficha anteriores) 3 
        (append (buscar-movimientos-vecinos tablero ficha (cons ficha anteriores) (append (list ficha) movimientos)) movimientos))]
    [(not (eq? (obtener-celda tablero (first ficha) (second ficha)) 2))
        (cond
        [(eq? (obtener-celda tablero (first ficha) (add1 (second ficha))) 2) 
            (buscar-inferior-derecho tablero (list (first ficha) (add1 (second ficha))) (cons ficha anteriores) (add1 contador) 
                (append (buscar-movimientos-vecinos tablero  (list (first ficha) (add1 (second ficha))) (cons ficha anteriores) (append (list ficha) movimientos)) movimientos))]
        [else(buscar-inferior-derecho tablero (list (first ficha) (add1 (second ficha))) (cons ficha anteriores) (add1 contador) movimientos)]) 
    ]
    [else(buscar-inferior-derecho tablero (list (first ficha) (add1 (second ficha))) (cons ficha anteriores) (add1 contador) 
    (append (buscar-movimientos-vecinos tablero (list (first ficha) (add1 (second ficha))) (cons ficha (cons (list (first ficha) (add1 (second ficha))) anteriores)) movimientos)  movimientos))]))


(define (buscar-inferior-izquierdo tablero ficha anteriores contador movimientos) ; II +1,0
    (cond 
    [(or
        (eq? contador 3)
        (miembro? anteriores ficha)
        (eq? (obtener-celda tablero (add1 (first ficha)) (second ficha)) 0))
         movimientos]
    [(and 
        (eq? contador 1)
        (eq? (obtener-celda tablero (first ficha) (second ficha)) 2))
    (buscar-inferior-izquierdo tablero (list (add1 (first ficha)) (second ficha)) (cons ficha anteriores) 3
        (append (buscar-movimientos-vecinos tablero ficha (cons ficha anteriores) (append (list ficha) movimientos)) movimientos))]
    [(and
        (eq? (obtener-celda tablero (first ficha) (second ficha)) 2)
        (eq? (obtener-celda tablero (add1 (first ficha)) (second ficha)) 2))
    (buscar-inferior-izquierdo tablero ficha (cons ficha (cons (list (add1 (first ficha)) (second ficha)) anteriores)) 3 
    (append (buscar-movimientos-vecinos tablero ficha (cons ficha (cons (list (add1 (first ficha)) (second ficha)) anteriores)) (append (list ficha) movimientos)) movimientos))]
    [(eq? (obtener-celda tablero (first ficha) (second ficha)) 2)
    (buscar-inferior-izquierdo tablero ficha (cons ficha anteriores) 3 
        (append (buscar-movimientos-vecinos tablero ficha (cons ficha anteriores) (append (list ficha) movimientos)) movimientos))]
    [(not (eq? (obtener-celda tablero (first ficha) (second ficha)) 2))
        (cond
        [(eq? (obtener-celda tablero (add1 (first ficha)) (second ficha)) 2) 
            (buscar-inferior-izquierdo tablero (list (add1 (first ficha)) (second ficha)) (cons ficha anteriores) (add1 contador) 
                (append (buscar-movimientos-vecinos tablero  (list (add1 (first ficha)) (second ficha)) (cons ficha anteriores) (append (list ficha) movimientos)) movimientos))]
        [else (buscar-inferior-izquierdo tablero (list (add1 (first ficha)) (second ficha)) (cons ficha anteriores) (add1 contador) movimientos)])]
    [else(buscar-inferior-izquierdo tablero (list (add1 (first ficha)) (second ficha)) (cons ficha anteriores) (add1 contador) 
    (append (buscar-movimientos-vecinos tablero (list (add1 (first ficha)) (second ficha)) (cons ficha anteriores) movimientos)  movimientos))]))

;Funciones de Interfaz
(define (mostrar-tablero tablero)
    (cond
    [(null? tablero) "Tablero Damas chinas"]
    [else (printf "~s\n" (first tablero)) (mostrar-tablero (rest tablero))]))
;Servicio telefónico: Aquí se llaman las funciones para probar.
;(mostrar-tablero (reverse (crear-tablero)))
; (mostrar-tablero (cargar-fichas (reverse (crear-tablero))))

;(obtener-celda (reverse (crear-tablero)) 7 6)
; (obtener-celda (reverse (crear-tablero)) 6 6)
; (obtener-celda (reverse (crear-tablero)) 7 4)
;(remove-duplicates (buscar-movimientos-vecinos (reverse (crear-tablero)) '(7 7) empty empty))
(remove-duplicates (buscar-movimientos-vecinos '((1 1 1 1 2 2 2 2 2)
					(1 1 1 2 2 2 2 2 2)
					(1 2 2 2 2 2 2 2 2)
					(1 2 2 2 2 2 1 2 2)
					(2 2 2 2 2 2 2 3 2)
					(2 2 2 2 2 2 2 2 2)
					(2 2 2 2 2 2 2 3 3)
					(2 2 2 2 2 2 3 3 3)
					(2 2 2 2 2 3 3 3 3)) '(7 7) empty empty))