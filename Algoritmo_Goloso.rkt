#lang scheme
(define matriz '((0 0 0 0 0 0 0 0)
                 (0 0 0 0 0 0 0 0)
                 (0 0 0 0 0 0 0 0)
                 (0 0 0 0 0 0 1 2)
                 (0 0 0 2 0 2 2 1)
                 (1 2 1 2 1 2 1 2)
                 (2 1 2 1 2 1 2 1)
                 (1 2 1 2 1 1 2 2)))

;;SIEMPRE (fila columna)
;;Obtiene los posibles candidatos
(define (conjuntoCandidatos matriz matrizRecorrida listaCandidatos fila)
  (cond
    ((null? matrizRecorrida) listaCandidatos)
    
    ((equal? (verifCeros (car matrizRecorrida)) #t)
         (conjuntoCandidatos matriz (cdr matrizRecorrida) listaCandidatos (+ fila 1)))
    
    ((and(equal? (verifNum (car matrizRecorrida)) #t) (equal? (miembro 0 (car (moverMatriz(matriz (- fila 1))))) #f))
     listaCandidatos)))
        
(define (conjuntoCandidatosAUX filaPorAnalizar fila listaCandidatos)
  (cond
    ((equal? (verifCeros filaPorAnalizar) #t) listaCandidatos)
    (else (cambiarValorEspecifico (- fila 1) (encontrarColumna filaPorAnalizar 0) listaCandidatos '() 1))))


          
(define (moverMatriz matriz fila)
  (cond
    ((equal? fila 0) matriz)
    (else (cdr matriz) (- fila 1))))

(define (cortar lista columna)
  (cond
    ((equal? columna 0) lista)
    (else(cortar (cdr lista) (- columna 1)))))

(cortar '(0 0 0 0 0 0 1 2) 3)

(define (verifSup lista columna)
  (cond
    ((null? lista) #f)
    ((and(equal? columna 0) (equal? (car lista) 0)) #t)
    (else (verifSup (cdr lista) (- columna 1)))))

(verifSup '(0 0 0 2 0 2 2 1) 4)
   
(define (encontrarColumna lista columna)
  (cond
    ((null? lista) columna)
    ((equal? (car lista) 0) (encontrarColumna (cdr lista) (+ columna 1)))
    (else (+ columna 1))))

(encontrarColumna '(0 0 0 2 0 2 2 1) 0)

;;Retorna TRUE si la fila tiene solamente 0
(define (verifCeros lista)
  (cond
    ((null? lista) #t)
    ((equal? (car lista) 0) (verifCeros (cdr lista)))
    (else #f)))

;;Retorna TRUE si la fila tiene solamente numeros > 0
(define (verifNum lista)
  (cond
    ((null? lista) #t)
    ((> (car lista) 0) (verifNum (cdr lista)))
    (else #f)))

;;Retorna TRUE si la fila tiene la palabra dada dentro de la lista
(define (miembro palabra lista)
  (cond
    ((null? lista)#f)
    ((equal? (car lista) palabra) #t)
     (else (miembro palabra (cdr lista)))))

;;Retorna una lista con el primer valor encontrado cambiado por un cero
(define (eliminaUnValor lista listaRespuesta)
  (cond
  ((null? lista) listaRespuesta)
  ((equal? (car lista) 0) (eliminaUnValor (cdr lista) (append listaRespuesta (list (car lista)))))
  (else (append listaRespuesta (list 0) (cdr lista)))))

(eliminaUnValor '(0 0 0 0 0 0 1 2) '())

;;Retorna la lista de candidatos renovada
(define (cambiarValorEspecifico fila columna lista listaRespuesta contador)
  (cond
  ((null? lista) listaRespuesta)
  ((equal? columna contador) (append listaRespuesta (list (list fila columna)) (cdr lista)))
  (else (cambiarValorEspecifico fila columna (cdr lista) (append listaRespuesta (list (car lista))) (+ contador 1)))))
                             

(quote "------------------------------------------------")

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


(define (traverse-matrix matrix)
  (for-each (lambda (row)
              (for-each (lambda (elem)
                          (display elem)
                          (display " "))
                        row)
              (newline))
            matrix))
