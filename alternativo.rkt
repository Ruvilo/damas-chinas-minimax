#lang racket

(require "damas.rkt")
;Funciones que creadoras

;; Parte Nueva --------(F)-------------
;#######################################################
;###################################################################################
(define (tablero-prueba) 
              '((3 3 3 3 2 2 2 2 2)
				    	  (3 3 3 2 2 2 2 2 2)
					      (3 3 2 2 2 1 2 2 2)
					      (3 2 2 2 2 2 2 2 2)
					      (2 2 2 2 2 1 2 2 2)   
					      (2 2 2 2 1 2 2 2 2)
					      (2 2 1 2 2 2 2 2 2)
					      (2 2 2 2 2 2 1 2 2)
					      (2 2 2 2 2 2 2 1 2))
)



(define (cargar-fichas tablero)   ; first para las fichas 3 y second para todas las fichas 1 
    (cargar-fichas-aux tablero 0 0 '() '()))

(define (cargar-fichas-aux tablero fila columna cpu player)
    (cond
    [(> fila 8) (cons player (list cpu))]
    [(> columna 8) (cargar-fichas-aux tablero (add1 fila) 0 cpu player)]
    [(eq? (obtener-celda tablero fila columna) 1) (cargar-fichas-aux tablero fila (+ columna 1) (append (list (list fila columna)) cpu) player)]; Esto mete las fichas de la computadora en la primera lista de las fichas
    [(eq? (obtener-celda tablero fila columna) 3) (cargar-fichas-aux tablero fila (+ columna 1) cpu (append (list (list fila columna)) player))]; Esto mete las fichas de la computadora en la segunda lista de las fichas
    [else( cargar-fichas-aux tablero fila (+ columna 1) cpu player)]))

;##########################################
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


;##########################################

;##########################################


 ;(mover (tablero-prueba) 3 '(7 6) '(7 5))


;(tablero-prueba)

;(first (cargar-fichas  (tablero-prueba))) 
;(eval (tablero-prueba) '(7 7) -1)
;(eval (tablero-prueba) '(8 6 ) -1)
;(eval (tablero-prueba) '(0 3  ) -1)
;(first (cargar-fichas  (tablero-prueba))) 



(eval-tablero (tablero-prueba) 3)