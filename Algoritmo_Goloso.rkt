#lang scheme
(define matriz '((0 0 0 0 0 0 0 0)
                 (0 0 0 0 0 0 0 0)
                 (0 0 0 0 0 0 0 0)
                 (0 0 0 0 0 0 1 2)
                 (0 0 0 2 0 2 2 1)
                 (1 2 1 2 1 2 1 2)
                 (2 1 2 1 2 1 2 1)
                 (1 2 1 2 1 1 2 2)))


;;Obtiene los posibles candidatos
(define (conjuntoCandidatos matriz matrizRecorrida listaCandidatos fila)
  (cond
    ((null? matrizRecorrida) listaCandidatos)
    ((and (equal? (verifCeros (car matrizRecorrida)) #t) (equal? (verifCeros (cadr matrizRecorrida)) #t))
         (conjuntoCandidatos matriz (cdr matrizRecorrida) listaCandidatos (+ fila 1)))
    
    ((equal? (verifSup (cortar matriz (- fila 1)) (encontrarColumna (car matrizRecorrida) 0)) #t)
     (conjuntoCandidatos matriz (cdr matrizRecorrida) (append listaCandidatos (list fila (encontrarColumna (car matrizRecorrida) 0)))))
    
    (else (conjuntoCandidatos matriz (cdr matrizRecorrida) listaCandidatos fila))))

(define (cortar lista fila)
  (cond
    ((equal? fila 0) lista)
    (else(cortar (cdr lista) (- fila 1)))))

(define (verifSup lista columna)
  (cond
    ((null? lista) #f)
    ((and(equal? columna 0) (equal? (car lista) 0)) #t)
    (else (verifSup (cdr lista) (- columna 1)))))
    
(define (encontrarColumna lista columna)
  (cond
    ((null? lista) columna)
    ((equal? (car lista) 0) (encontrarColumna (cdr lista) (+ columna 1)))
    (else (+ columna 1))))

(define (verifCeros lista)
  (cond
    ((null? lista) #t)
    ((equal? (car lista) 0) (verifCeros (cdr lista)))
    (else #f)))

(conjuntoCandidatos matriz matriz '() 1)

;;Posibles pesos:
;;Pts al rededor de ia ( +1 )
;;Pts al rededor de jugador ( +2 )
;;Posicion // #eliminados +1
;;(define (Funcion_Objetivo matriz matrizRecorrida contador listaPesos listaCandidatos)
  ;;(cond
    ;;)) ;;Asigna pesos a las posibles soluciones

;;(define (Funcion_Viabilidad)) ;;Analiza si el candidato seleccionado sirve para obtener una solución

;;(define (Funcion_Seleccion)) ;;Selecciona el mejor candidato



;;(define (Funcion_Solucion)) ;;ANALIZA cuando cualquiera de los dos gana

;;(define (Cuantos_Conectados))

(cdr matriz)


(define (traverse-matrix matrix)
  (for-each (lambda (row)
              (for-each (lambda (elem)
                          (display elem)
                          (display " "))
                        row)
              (newline))
            matrix))
