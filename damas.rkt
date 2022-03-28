#lang racket
(require 2htdp/image)

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
    [(null? tablero) "Ingrese donde quiere mover"]
    [else (printf "~s\n" (first tablero)) (mostrar-tablero (rest tablero))]))

;Funcionalidad Jogo

(define (cargar-movimientos tablero ficha)
    (cargar-movimientos-aux tablero ficha (buscar-movimientos-vecinos tablero ficha)))

(define (cargar-movimientos-aux tablero ficha movimientos)
    (cond
    [(empty? movimientos) tablero]
    [else(cargar-movimientos-aux 
        (list-set tablero (first (first movimientos)) (list-set (list-ref tablero (first (first movimientos))) (second (first movimientos)) 4))
        ficha (rest movimientos))]))

(define celda
           (square 70 "outline" "blue"))
(define celda-ficha
    (overlay/align "center" "center" (circle 15 "solid" "yellow") celda))

(define (ficha digit)
     (cond
        [(= digit 1) (overlay
     (text (number->string digit) 16 "black")
     (circle 20 "solid" "red"))]
        [(= digit 3) (overlay
     (text (number->string digit) 16 "black")
     (circle 20 "solid" "blue"))]
        [(= digit 4) (overlay
     (text (number->string digit) 16 "black")
     (circle 20 "solid" "green"))]
        [else(overlay
     (text (number->string digit) 16 "black")
     (circle 20 "solid" "grey")) ] ; es min
    )
     
     )

(define (borde num)
    (overlay
     (text (number->string num) 16 "white")
     (circle 20 "outline" "white"))
      
    )



(define (columnas num num-fila)
  (beside   (borde num-fila) (ficha (first num))   (ficha (second num)) (ficha (third num)) (ficha ( fourth num)) (ficha (fifth num)) (ficha (sixth num)) (ficha (seventh num)) (ficha ( eighth num)) (ficha ( ninth num)) (borde num-fila)) 
)


(define (filas num num-fila)
  (cond
    [(eq? num-fila 0)
     (above (beside (borde 0) (borde 1) (borde 2) (borde 3) (borde 4) (borde 5) (borde 6) (borde 7) (borde 8)) (columnas num num-fila))]
    [else (  columnas num num-fila)])
           
)

(define (mostrar-interfaz tablero)

  (rotate -45(above (filas (first tablero) 0)  
  (filas (second tablero) 1)
  (filas (third tablero) 2) 
(filas ( fourth tablero) 3) 
(filas (fifth tablero) 4) 
(filas (sixth tablero) 5) 
(filas (seventh tablero) 6) 
(filas ( eighth tablero) 7)
  (filas ( ninth tablero) 8)
  (beside (borde 0) (borde 1) (borde 2) (borde 3) (borde 4) (borde 5) (borde 6) (borde 7) (borde 8))
  ))
)

(mostrar-interfaz  (cargar-movimientos '((1 1 1 1 2 2 2 2 2)
					(1 1 1 2 2 2 2 2 2)
					(1 2 2 2 2 2 2 2 2)
					(1 2 2 2 2 2 1 2 2)
					(2 2 2 2 2 2 2 3 2)
					(2 2 2 2 2 2 2 2 2)
					(2 2 2 2 2 2 2 3 3)
					(2 2 2 2 2 2 3 3 3)
					(2 2 2 2 2 3 3 3 3)) '(7 7) ))

(display "Ingrese la fila del campo: ")
(define fila (read))
(display "Ingrese la columna del campo: ")
(define columna (read))


(list fila columna)



