#lang racket/gui
(require "damas.rkt")
 (require racket/draw)
(require (lib "graphics.ss" "graphics"))
(open-graphics)
(define ventana (open-viewport "Damas Chinas contra Sufeiya" 650 700))
(define oculta (open-pixmap "Secreto" 650 700))

(define (cargar-celdas tablero)
    (cargar-celdas-aux tablero 0 0 305 50 empty empty))

(define (cargar-celdas-aux tablero fila columna x y mapa fila-mapa)
    (cond
    [(= fila (length tablero)) mapa]
    [(= columna (length tablero)) (cargar-celdas-aux tablero (add1 fila) 0 (- x 200) (- y 280) (append mapa fila-mapa) empty)]
    [else
        (cargar-celdas-aux tablero fila (add1 columna) (+ x 20) (+ y 35) mapa (append fila-mapa (list (list x y))))]))


(define (crear-mapa-interfaz tablero)
    (crear-mapa-interfaz-aux tablero 0 0 305 50 empty empty))

(define (crear-mapa-interfaz-aux tablero fila columna x y mapa fila-mapa)
    (cond
    [(= fila (length tablero)) mapa]
    [(= columna (length tablero)) (crear-mapa-interfaz-aux tablero (add1 fila) 0 (- x 200) (- y 280) (append mapa (list fila-mapa)) empty)]
    [else
        (crear-mapa-interfaz-aux tablero fila (add1 columna) (+ x 20) (+ y 35) mapa (append fila-mapa (list (list x y))))]))

(define (buscar-indice-coordenada mapa coordenada)
    (buscar-indice-coordenada-aux mapa coordenada 0))

(define (buscar-indice-coordenada-aux mapa coordenada fila)
    (cond
    [(eq? fila 9) #f]
    [(not (eq? (index-of (list-ref mapa fila) coordenada) #f)) (list fila (index-of (list-ref mapa fila) coordenada))]
    [else(buscar-indice-coordenada-aux mapa coordenada (add1 fila))]))

(define (mostrar-interfaz tablero)
    (mostrar-interfaz-aux tablero 0 0 305 50))

(define (mostrar-interfaz-aux tablero fila columna x y)
    (cond
    [(= fila (length tablero)) (void)]
    [(= columna (length tablero)) (mostrar-interfaz-aux tablero (add1 fila) 0 (- x 200) (- y 280))]
    [else
        (renderizar-ficha (obtener-celda tablero fila columna) x y)
        (mostrar-interfaz-aux tablero fila (add1 columna) (+ x 20) (+ y 35))]))

(define (renderizar-ficha valor x y)
    (cond
        [(= valor 1) 
            ((draw-solid-ellipse oculta) (make-posn x y) 40 40 "lightcoral")]
        [(= valor 3) 
            ((draw-solid-ellipse oculta) (make-posn x y) 40 40 "deepskyblue")]
        [(= valor 4) 
            ((draw-solid-ellipse oculta) (make-posn x y) 40 40 "lightgreen")]
        [else((draw-solid-ellipse oculta) (make-posn x y) 40 40 "Gainsboro")]))

(define (marca x y lado tablero)
    (cond
    [(equal? lado 'arriba)
        (mostrar-interfaz tablero)
        ((draw-ellipse oculta) (make-posn x y) 40 40 "black")
        (copy-viewport oculta ventana)
        ((clear-viewport oculta))
        ]
    [(equal? lado 'abajo)
        (mostrar-interfaz tablero)
        ((draw-ellipse oculta) (make-posn x y) 40 40 "black")
        (copy-viewport oculta ventana)
        ((clear-viewport oculta))
    ]
    [(equal? lado 'derecha)
        (mostrar-interfaz tablero)
        ((draw-ellipse oculta) (make-posn x y) 40 40 "black")
        (copy-viewport oculta ventana)
        ((clear-viewport oculta))
        ]
    [(equal? lado 'izquierda)
        (mostrar-interfaz tablero)
        ((draw-ellipse oculta) (make-posn x y) 40 40 "black")
        (copy-viewport oculta ventana)
        ((clear-viewport oculta))
        ]
    [(equal? lado 'espacio)
        (mostrar-interfaz tablero)]
    [(equal? lado 'enter)
        (mostrar-interfaz tablero)]
    [else (void)]))

(define (teclado x y tecla tablero mapa ficha t-original)
    (cond
    [(miembro? (cargar-celdas tablero) (list x y)) 
    (cond
    [(equal? tecla 'up)
        (marca (+ x 20) (- y 35) 'arriba tablero)
        (teclado (+ x 20) (- y 35) (key-value (get-key-press ventana)) tablero mapa ficha t-original)]
    [(equal? tecla 'down)
        (marca (- x 20) (+ y 35) 'abajo tablero)
        (teclado (- x 20) (+ y 35) (key-value (get-key-press ventana)) tablero mapa ficha t-original)]
    [(equal? tecla 'right)
        (marca (+ x 40) y 'derecha tablero)
        (teclado (+ x 40) y (key-value (get-key-press ventana)) tablero mapa ficha t-original)]
    [(equal? tecla 'left)
        (marca (- x 40) y 'izquierda tablero)
        (teclado (- x 40) y (key-value (get-key-press ventana)) tablero mapa ficha t-original)]
    [(and 
        (equal? tecla #\space)
        (miembro? (first (cargar-fichas t-original)) (buscar-indice-coordenada mapa (list x y))))
        (marca x y 'espacio (cargar-movimientos t-original (buscar-indice-coordenada mapa (list x y))))
        (teclado x y (key-value (get-key-press ventana)) (cargar-movimientos t-original (buscar-indice-coordenada mapa (list x y))) mapa (buscar-indice-coordenada mapa (list x y)) t-original)]
    [(equal? tecla #\space)
        (cond
        [(and 
            (empty? ficha)
            (miembro? (first (cargar-fichas t-original)) (buscar-indice-coordenada mapa (list x y))))
                (marca x y 'espacio (cargar-movimientos t-original (buscar-indice-coordenada mapa (list x y))))
                (teclado x y (key-value (get-key-press ventana)) (cargar-movimientos t-original (buscar-indice-coordenada mapa (list x y))) mapa (buscar-indice-coordenada mapa (list x y)) t-original)]
        [(and 
            (not (empty? ficha))
            (list? (mover t-original 3 ficha (buscar-indice-coordenada mapa (list x y)))))
                (marca x y 'enter (mover t-original 3 ficha (buscar-indice-coordenada mapa (list x y))))
                (teclado x y (key-value (get-key-press ventana)) 
                    (mover t-original 3 ficha (buscar-indice-coordenada mapa (list x y))) mapa empty (mover t-original 3 ficha (buscar-indice-coordenada mapa (list x y))))]
        [else 
            (marca x y 'arriba tablero)
            (teclado x y (key-value (get-key-press ventana)) tablero mapa ficha t-original)])]
    [else 
            (marca x y 'arriba tablero)
            (teclado x y (key-value (get-key-press ventana)) tablero mapa ficha t-original)])]
    
    [else
        (marca 305 330 'arriba tablero)
        (teclado 305 330 (key-value (get-key-press ventana)) t-original mapa ficha t-original)]))
(teclado 305 330 'control (crear-tablero) (crear-mapa-interfaz (crear-tablero)) empty (crear-tablero))
;(mostrar-interfaz (crear-tablero))

;(miembro? (cargar-celdas (crear-tablero)) (list 325 50))

; (mostrar-tablero (crear-mapa-interfaz (crear-tablero))) ;retorna una lista con todas las coordenadas de todo el tablero
; (buscar-indice-coordenada (crear-mapa-interfaz (crear-tablero)) (list 305 630))
