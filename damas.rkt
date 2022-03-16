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

(mostrar-tablero (crear-tablero))