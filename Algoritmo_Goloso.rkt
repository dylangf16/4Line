#lang scheme
(quote "buenas")

;;(define (Funcion_Objetivo)) ;;Asigna pesos a las posibles soluciones

;;(define (Conjunto_Candidatos)) ;;Obtiene los posibles candidatos

;;(define (Funcion_Viabilidad)) ;;Analiza si el candidato seleccionado sirve para obtener una solución

;;(define (Funcion_Seleccion)) ;;Selecciona el mejor candidato



;;(define (Funcion_Solucion)) ;;ANALIZA cuando cualquiera de los dos gana

;;(define (Cuantos_Conectados))

(define matriz '((2 2 1 1 2 1 2 1)
                (1 2 1 2 1 2 1 2)
                (2 1 2 1 2 1 2 1)
                (1 2 2 0 2 0 0 0)
                (2 1 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0)))


(define (traverse-matrix matrix)
  (for-each (lambda (row)
              (for-each (lambda (elem)
                          (display elem)
                          (display " "))
                        row)
              (newline))
            matrix))


(traverse-matrix matriz)