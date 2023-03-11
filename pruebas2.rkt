#lang racket



(require "Manipulacion_matriz.rkt")
(require "Miscelaneos.rkt")
;;(require "Greedy_algorithm.rkt")


(define matrix (buildMatrix '() 8 8))
(display matrix)

;;fila 5 // columna 2
(set! matrix (replaceInMatrix matrix 4 1 4512 0))
(quote "sis")
(display matrix)