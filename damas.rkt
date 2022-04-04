#lang racket/gui
(require 2htdp/image)
(provide crear-tablero)
(provide obtener-celda)
(provide mostrar-tablero)
(provide cargar-movimientos)
(provide cargar-fichas)
(provide miembro?)
(provide mover)

#|
    Proyecto 01: Minimax - Damas Chinas
    Rubén Augusto Villalta López - 2017130955
    Ricardo Daniel Loaiza Rojas - 2017076031
    Fernando André Binda Sanchez - 2017096959
|#

;Funciones que creadoras

;Esta función se encarga de crear un tablero de 9x9 para dos jugadores de damas 
; chinas
;E: None
;S: tablero:list
(define (crear-tablero)
    (reverse (crear-tablero-aux empty 4)))

;Esta es una función auxiliar para crear el tablero
;E: tablero:list, fichas:int
;S: tablero:list
(define (crear-tablero-aux tablero fichas)
    (cond
    [(>= (length tablero) 9) tablero]
    [(< (length tablero) 4) (crear-tablero-aux (cons (append (llenar-lista fichas 1) (llenar-lista (- 9 fichas) 2)) tablero) (sub1 fichas))]
    [else(crear-tablero-aux (cons (append (llenar-lista (- 9 fichas) 2) (llenar-lista fichas 3)) tablero) (add1 fichas))]))   

;Esta funcion crea una lista con la cantidad y con el elemento seleccionado
;E: cantidad:int, elemento:int
;S: lista:list
(define (llenar-lista cantidad elemento)
    (cond
    [(<= cantidad 0) '()]
    [else (llenar-lista-aux cantidad elemento '())]))

;Esta funcion auxiliar crea una lista que se llenan con mismo elemento una 
;cantidad de veces
;E: cantidad:int, elemento:int
;S: lista:list
(define (llenar-lista-aux cantidad elemento nueva)
    (cond
    [(eq? cantidad 0) nueva]
    [else (llenar-lista-aux (- cantidad 1) elemento (cons elemento nueva))]))
;Esta funcion busca las posiciones de fichas del jugador y de la inteligncia 
;artificial y las guarda en una lista
;E: tablero:list
;S: fichas:list
(define (cargar-fichas tablero)
    (cargar-fichas-aux tablero 0 0 '() '()))

;Esta funcion auxiliar busca las fichas del jugador humano y el jugador IA.
;E: tablero:list, fila:int, columna:int, cpu:list, player:list
;S: fichas:list 
(define (cargar-fichas-aux tablero fila columna cpu player)
    (cond
    [(> fila 8) (cons player (list cpu))]
    [(> columna 8) (cargar-fichas-aux tablero (add1 fila) 0 cpu player)]
    [(eq? (obtener-celda tablero fila columna) 1) (cargar-fichas-aux tablero fila (+ columna 1) (append (list (list fila columna)) cpu) player)]; Esto mete las fichas de la computadora en la primera lista de las fichas
    [(eq? (obtener-celda tablero fila columna) 3) (cargar-fichas-aux tablero fila (+ columna 1) cpu (append (list (list fila columna)) player))]; Esto mete las fichas de la computadora en la segunda lista de las fichas
    [else( cargar-fichas-aux tablero fila (+ columna 1) cpu player)]))

;Esta funcion obtiene una celda del tablero en una posición especifica.
;E: tablero:list, fila:int, columna:int
;S: celda:int
(define (obtener-celda tablero fila columna)
    (cond
    [(or 
        (< fila 0)
        (< columna 0)
        (> fila (sub1 (length tablero)))
        (> columna (sub1 (length tablero)))) 0]
    [else(list-ref (list-ref tablero fila) columna)]))

;Funciones de movimientos

;Esta funcion verifica si un elemento forma parte de una lista
;E: lista:list, elemento:int/list
;S: true:bool false:bool
(define (miembro? lista elemento)
  (cond
    [(not (eq? (member elemento lista) #f)) #t]
    [else #f]))


;Esta funcion busca los posibles movimientos de una ficha
;E: tablero:list, ficha:list
;S: movimientos:list   
(define (buscar-movimientos-vecinos tablero ficha )
    (remove-duplicates (buscar-movimientos-vecinos-aux  tablero ficha empty empty)))

(define (buscar-movimientos-vecinos-aux  tablero ficha anteriores movimientos);
    (append 
        (buscar-superior-derecho  tablero ficha anteriores 0 movimientos)
        (buscar-superior-izquierdo  tablero ficha anteriores 0 movimientos)
        (buscar-izquierdo tablero ficha anteriores 0 movimientos)
        (buscar-inferior-izquierdo tablero ficha anteriores 0 movimientos)
        (buscar-inferior-derecho  tablero ficha anteriores 0 movimientos)
        (buscar-derecho tablero ficha anteriores 0 movimientos)
        movimientos))

;Esta funcion mueve un pivote de manera superior derecha para ir verificando
;si esta en una posición valida para mover la ficha y se guarda es coordenada
;E: tablero:list, ficha:list, anteriores:list, contador:int, movimientos:list
;S: movimientos:list
(define (buscar-superior-derecho  tablero ficha anteriores contador movimientos); SD -1,0
    (cond
    [(or 
        (> contador 1)
        (eq? (obtener-celda tablero (sub1 (first ficha)) (second ficha)) 0)
        (miembro? anteriores ficha))
    movimientos]
    [(eq? (obtener-celda tablero (first ficha) (second ficha)) 2)
        (cond 
        [(eq? (obtener-celda tablero (sub1 (first ficha)) (second ficha)) 2)
            (buscar-superior-derecho  tablero ficha (cons ficha anteriores) (add1 contador) movimientos)]
        [else(buscar-superior-derecho  tablero (list (sub1 (first ficha)) (second ficha)) (cons ficha anteriores) (add1 contador) movimientos)])]
    [(eq? (obtener-celda tablero (sub1 (first ficha)) (second ficha)) 2)
        (cond 
        [(eq? contador 0)
            (buscar-superior-derecho  tablero ficha (cons ficha anteriores) 2 (append (list (list (sub1 (first ficha)) (second ficha))) movimientos))]
        [else(buscar-superior-derecho  tablero (list (sub1 (first ficha)) (second ficha)) (cons ficha anteriores) 0 
            (append (buscar-movimientos-vecinos-aux  tablero (list (sub1 (first ficha)) (second ficha)) (cons ficha anteriores) (append (list (list (sub1 (first ficha)) (second ficha))) movimientos)) movimientos))])]
    [else(buscar-superior-derecho  tablero (list (sub1 (first ficha)) (second ficha)) (cons ficha anteriores) (add1 contador) movimientos)]))

;Esta funcion mueve un pivote de manera superior izquierda para ir verificando
;si esta en una posición valida para mover la ficha y se guarda es coordenada
;E: tablero:list, ficha:list, anteriores:list, contador:int, movimientos:list
;S: movimientos:list
(define (buscar-superior-izquierdo  tablero ficha anteriores contador movimientos);SI 0,-1
    (cond
    [(or 
        (> contador 1)
        (eq? (obtener-celda tablero (first ficha) (sub1 (second ficha))) 0)
        (miembro? anteriores ficha))
    movimientos]
    [(eq? (obtener-celda tablero (first ficha) (second ficha)) 2)
        (cond 
        [(eq? (obtener-celda tablero (first ficha) (sub1 (second ficha))) 2)
            (buscar-superior-izquierdo  tablero ficha (cons ficha anteriores) (add1 contador) movimientos)]
        [else(buscar-superior-izquierdo  tablero (list (first ficha) (sub1 (second ficha))) (cons ficha anteriores) (add1 contador) movimientos)])]
    [(eq? (obtener-celda tablero (first ficha) (sub1 (second ficha))) 2)
        (cond 
        [(eq? contador 0)
            (buscar-superior-izquierdo  tablero ficha (cons ficha anteriores) 2 (append (list (list (first ficha) (sub1 (second ficha)))) movimientos))]
        [else(buscar-superior-izquierdo  tablero (list (first ficha) (sub1 (second ficha))) (cons ficha anteriores) 0 
            (append (buscar-movimientos-vecinos-aux  tablero (list (first ficha) (sub1 (second ficha))) (cons ficha anteriores) (append (list (list (first ficha) (sub1 (second ficha)))) movimientos)) movimientos))])]
    [else(buscar-superior-izquierdo  tablero (list (first ficha) (sub1 (second ficha))) (cons ficha anteriores) (add1 contador) movimientos)]))

;Esta funcion mueve un pivote de manera izquierda para ir verificando
;si esta en una posición valida para mover la ficha y se guarda es coordenada
;E: tablero:list, ficha:list, anteriores:list, contador:int, movimientos:list
;S: movimientos:list
(define (buscar-izquierdo tablero ficha anteriores contador movimientos);I +1,-1
    (cond
    [(or 
        (> contador 1)
        (eq? (obtener-celda tablero (add1 (first ficha)) (sub1 (second ficha))) 0)
        (miembro? anteriores ficha))
    movimientos]
    [(eq? (obtener-celda tablero (first ficha) (second ficha)) 2)
        (cond 
        [(eq? (obtener-celda tablero (add1 (first ficha)) (sub1 (second ficha))) 2)
            (buscar-izquierdo tablero ficha (cons ficha anteriores) (add1 contador) movimientos)]
        [else(buscar-izquierdo tablero (list (add1 (first ficha)) (sub1 (second ficha))) (cons ficha anteriores) (add1 contador) movimientos)])]
    [(eq? (obtener-celda tablero (add1 (first ficha)) (sub1 (second ficha))) 2)
        (cond 
        [(eq? contador 0)
            (buscar-izquierdo tablero ficha (cons ficha anteriores) 2 (append (list (list (add1 (first ficha)) (sub1 (second ficha)))) movimientos))]
        [else(buscar-izquierdo tablero (list (add1 (first ficha)) (sub1 (second ficha))) (cons ficha anteriores) 0 
            (append (buscar-movimientos-vecinos-aux  tablero (list (add1 (first ficha)) (sub1 (second ficha))) (cons ficha anteriores) (append (list (list (add1 (first ficha)) (sub1 (second ficha)))) movimientos)) movimientos))])]
    [else(buscar-izquierdo tablero (list (add1 (first ficha)) (sub1 (second ficha))) (cons ficha anteriores) (add1 contador) movimientos)]))

;Esta funcion mueve un pivote de manera inferior izquierda para ir verificando
;si esta en una posición valida para mover la ficha y se guarda es coordenada
;E: tablero:list, ficha:list, anteriores:list, contador:int, movimientos:list
;S: movimientos:list
(define (buscar-inferior-izquierdo tablero ficha anteriores contador movimientos);II +1, 0
    (cond
    [(or 
        (> contador 1)
        (eq? (obtener-celda tablero (add1 (first ficha)) (second ficha)) 0)
        (miembro? anteriores ficha))
    movimientos]
    [(eq? (obtener-celda tablero (first ficha) (second ficha)) 2)
        (cond 
        [(eq? (obtener-celda tablero (add1 (first ficha)) (second ficha)) 2)
            (buscar-inferior-izquierdo tablero ficha (cons ficha anteriores) (add1 contador) movimientos)]
        [else(buscar-inferior-izquierdo tablero (list (add1 (first ficha)) (second ficha)) (cons ficha anteriores) (add1 contador) movimientos)])]
    [(eq? (obtener-celda tablero (add1 (first ficha)) (second ficha)) 2)
        (cond 
        [(eq? contador 0)
            (buscar-inferior-izquierdo tablero ficha (cons ficha anteriores) 2 (append (list (list (add1 (first ficha)) (second ficha))) movimientos))]
        [else(buscar-inferior-izquierdo tablero (list (add1 (first ficha)) (second ficha)) (cons ficha anteriores) 0 
            (append (buscar-movimientos-vecinos-aux  tablero (list (add1 (first ficha)) (second ficha)) (cons ficha anteriores) (append (list (list (add1 (first ficha)) (second ficha))) movimientos)) movimientos))])]
    [else(buscar-inferior-izquierdo tablero (list (add1 (first ficha)) (second ficha)) (cons ficha anteriores) (add1 contador) movimientos)]))

;Esta funcion mueve un pivote de manera inferior derecha para ir verificando
;si esta en una posición valida para mover la ficha y se guarda es coordenada
;E: tablero:list, ficha:list, anteriores:list, contador:int, movimientos:list
;S: movimientos:list
(define (buscar-inferior-derecho  tablero ficha anteriores contador movimientos);ID 0,+1
    (cond
    [(or 
        (> contador 1)
        (eq? (obtener-celda tablero (first ficha) (add1 (second ficha))) 0)
        (miembro? anteriores ficha))
    movimientos]
    [(eq? (obtener-celda tablero (first ficha) (second ficha)) 2)
        (cond 
        [(eq? (obtener-celda tablero (first ficha) (add1 (second ficha))) 2)
            (buscar-inferior-derecho  tablero ficha (cons ficha anteriores) (add1 contador) movimientos)]
        [else(buscar-inferior-derecho  tablero (list (first ficha) (add1 (second ficha))) (cons ficha anteriores) (add1 contador) movimientos)])]
    [(eq? (obtener-celda tablero (first ficha) (add1 (second ficha))) 2)
        (cond 
        [(eq? contador 0)
            (buscar-inferior-derecho  tablero ficha (cons ficha anteriores) 2 (append (list (list (first ficha) (add1 (second ficha)))) movimientos))]
        [else(buscar-inferior-derecho  tablero (list (first ficha) (add1 (second ficha))) (cons ficha anteriores) 0 
            (append (buscar-movimientos-vecinos-aux  tablero (list (first ficha) (add1 (second ficha))) (cons ficha anteriores) (append (list (list (first ficha) (add1 (second ficha)))) movimientos)) movimientos))])]
    [else(buscar-inferior-derecho  tablero (list (first ficha) (add1 (second ficha))) (cons ficha anteriores) (add1 contador) movimientos)]))

;Esta funcion mueve un pivote de manera derecha para ir verificando si esta en
;una posición valida para mover la ficha y se guarda es coordenada
;E: tablero:list, ficha:list, anteriores:list, contador:int, movimientos:list
;S: movimientos:list
(define (buscar-derecho tablero ficha anteriores contador movimientos);D -1,+1
    (cond
    [(or 
        (> contador 1)
        (eq? (obtener-celda tablero (sub1 (first ficha)) (add1 (second ficha))) 0)
        (miembro? anteriores ficha))
    movimientos]
    [(eq? (obtener-celda tablero (first ficha) (second ficha)) 2)
        (cond 
        [(eq? (obtener-celda tablero (sub1 (first ficha)) (add1 (second ficha))) 2)
            (buscar-derecho tablero ficha (cons ficha anteriores) (add1 contador) movimientos)]
        [else(buscar-derecho tablero (list (sub1 (first ficha)) (add1 (second ficha))) (cons ficha anteriores) (add1 contador) movimientos)])]
    [(eq? (obtener-celda tablero (sub1 (first ficha)) (add1 (second ficha))) 2)
        (cond 
        [(eq? contador 0)
            (buscar-derecho tablero ficha (cons ficha anteriores) 2 (append (list (list (sub1 (first ficha)) (add1 (second ficha)))) movimientos))]
        [else(buscar-derecho tablero (list (sub1 (first ficha)) (add1 (second ficha))) (cons ficha anteriores) 0 
            (append (buscar-movimientos-vecinos-aux  tablero (list (sub1 (first ficha)) (add1 (second ficha))) (cons ficha anteriores) (append (list (list (sub1 (first ficha)) (add1 (second ficha)))) movimientos)) movimientos))])]
    [else(buscar-derecho tablero (list (sub1 (first ficha)) (add1 (second ficha))) (cons ficha anteriores) (add1 contador) movimientos)]))

;Funciones de Interfaz
;Esta funcion muesta una matriz de una manera muy bonita
;E: tablero:lista
;S: prints
(define (mostrar-tablero tablero)
    (cond
    [(null? tablero) "Tablero Damas contra Sufeiya"]
    [else (printf "~s\n" (first tablero)) (mostrar-tablero (rest tablero))]))

;Funcionalidad Juego

;Funcion que carga los movimientos posibles en el tablero
;E: tablero, ficha
;S: tablero modificado
(define (cargar-movimientos tablero ficha)
    (cargar-movimientos-aux tablero ficha (buscar-movimientos-vecinos tablero ficha)))

(define (cargar-movimientos-aux tablero ficha movimientos)
    (cond
    [(empty? movimientos) tablero]
    [else(cargar-movimientos-aux 
        (list-set tablero (first (first movimientos)) (list-set (list-ref tablero (first (first movimientos))) (second (first movimientos)) 4))
        ficha (rest movimientos))]))

;Funcion que mueve la ficha atraves del tablero, estableciendo la posicion de origen a un estado 2 (vacia)
;E: tablero, turno, ficha, campo (lugar al que va la ficha)
;S: tablero modificado
(define (mover tablero turno ficha campo)
    (mover-aux tablero turno ficha campo (buscar-movimientos-vecinos tablero ficha)))

(define (mover-aux tablero turno ficha campo movimientos)
    (cond
    [(and 
        (miembro? movimientos campo)
        (not (empty? ficha))) 
        (list-set (vaciar-campo tablero ficha) (first campo) (list-set (list-ref (vaciar-campo tablero ficha) (first campo)) (second campo) turno))]
    [else #f]))

;Funcion que establece la posicion dada a un estado 2 (vacia)
;E: tablero, ficha (lugar de la ficha)
;S: tablero modificado
(define (vaciar-campo tablero ficha)
    (list-set tablero (first ficha) (list-set (list-ref tablero (first ficha)) (second ficha) 2)))

;Funcion que cambia los turnos, 1 para rojas, 3 para azules
;E: turno
;S: int, el siguiente numero (1 -> 3 | 3 -> 1)
(define (cambiar-turno turno)
    (cond
    [(eq? turno 1) 3]
    [else 1]))



;#####################################################################
; MinMax


;Funciones minimax de Sufeiya
(define (cargar-movimientos-fichas tablero)
    (cargar-movimientos-fichas-aux tablero 1 (second (cargar-fichas tablero)) empty empty empty))

(define (cargar-movimientos-fichas-aux tablero turno fichas tableros ficha movimientos)
    (cond
    [(empty? fichas) tableros]
    [(empty? movimientos) (cargar-movimientos-fichas-aux tablero turno (rest fichas) tableros (first fichas) (buscar-movimientos-vecinos tablero (first fichas)))]
    [else (cargar-movimientos-fichas-aux tablero turno fichas (append  tableros  (list (mover tablero turno ficha (first movimientos)))) ficha (rest movimientos))]))

; Eval 

 (define (eval tablero ficha turno)
        (define (eval_aux tablero ficha turno meta) 
            (+ (- (first meta) (* turno (first ficha))) (- (second meta) (* turno (second ficha))))                       
        ) 
        ; Funcion Eval: evalua la cercania de una ficha con su meta al otro lado del tablero 
        ; Entre menor sea su resultado , mejor
        ; Entradas: el tablero , la ficha a evaluar y un turno (1 o -1)
      (cond
        [(= turno 1) (eval_aux tablero ficha turno `(8 8))] ; turno positivo , El 1 esta jugando 
        [else ( eval_aux tablero ficha turno `(0 0))] ; turno negativo , El 3 esta jugando
       )
  )

(define (eval-tablero tablero turno) 
    (cond
    [(= turno 3) (eval-tablero-aux tablero -1 (first (cargar-fichas tablero)) 0)]
    [else (eval-tablero-aux tablero 1 (second (cargar-fichas tablero)) 0)]))

(define (eval-tablero-aux tablero turno fichas total) 
    (cond
    [(empty? fichas) (- total 20)] ; retorna 0 cuando el jugador gana , entre mas cercano a 0 ... mejor es el tablero 
    [else (eval-tablero-aux tablero turno (rest fichas) (+ (eval tablero (first fichas) turno) total))]))


