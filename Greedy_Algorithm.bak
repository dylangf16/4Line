#lang scheme
(define matriz '((0 0 0 0 0 0 0 0)
                 (0 0 0 0 0 0 0 0)
                 (0 0 0 0 0 0 0 0)
                 (0 0 0 0 0 0 1 2)
                 (0 0 0 2 0 2 2 1)
                 (1 0 1 2 1 2 1 2)
                 (2 0 2 1 2 1 2 1)
                 (1 2 1 2 1 1 2 2)))

;;SIEMPRE (fila columna)
;;Obtiene los posibles candidatos
(define (conjuntoCandidatos matriz matrizRecorrida listaCandidatos fila)
  (cond
    ((null? matrizRecorrida) listaCandidatos) ;;salida
    
    ((equal? (verifCeros (car matrizRecorrida)) #t)
         (conjuntoCandidatos matriz (cdr matrizRecorrida) listaCandidatos (+ fila 1))) ;;verif si la fila está llena de 0
    
    ((equal? (verifNum (car matrizRecorrida)) #t)
     (conjuntoCandidatos matriz (cdr matrizRecorrida) (conjuntoCandidatosAUX (car matrizRecorrida) fila listaCandidatos) (+ fila 1))) ;;verif si la fila está llena de números
    
    (else (conjuntoCandidatos matriz (cdr matrizRecorrida) (conjuntoCandidatosAUX (car matrizRecorrida) fila listaCandidatos) (+ fila 1)))))
        
(define (conjuntoCandidatosAUX filaPorAnalizar fila listaCandidatos)
  (cond
    ((equal? (verifCeros filaPorAnalizar) #t) listaCandidatos)
    (else (conjuntoCandidatosAUX (eliminaUnValor filaPorAnalizar '()) fila (cambiarValorEspecifico (- fila 1) (encontrarColumna filaPorAnalizar 0) listaCandidatos '() 1)))))

;;Función que retorna la columna del primer valor que se encuentra
(define (encontrarColumna lista columna)
  (cond
    ((null? lista) columna)
    ((equal? (car lista) 0) (encontrarColumna (cdr lista) (+ columna 1)))
    (else (+ columna 1))))

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

;;Retorna una lista con el primer valor encontrado cambiado por un cero
(define (eliminaUnValor lista listaRespuesta)
  (cond
  ((null? lista) listaRespuesta)
  ((equal? (car lista) 0) (eliminaUnValor (cdr lista) (append listaRespuesta (list (car lista)))))
  (else (append listaRespuesta (list 0) (cdr lista)))))

;;Retorna la lista de candidatos renovada
(define (cambiarValorEspecifico fila columna lista listaRespuesta contador)
  (cond
  ((null? lista) listaRespuesta)
  ((and (equal? columna contador) (equal? (car lista) 0)) (append listaRespuesta (list (list fila columna)) (cdr lista)))
  (else (cambiarValorEspecifico fila columna (cdr lista) (append listaRespuesta (list (car lista))) (+ contador 1)))))

(conjuntoCandidatos matriz matriz '(0 0 0 0 0 0 0 0) 1)


;;Posibles pesos:
;;Ficha de ia ( +4 ) Ficha ia = 2
;;Ficha de jugador ( +8 ) Ficha jugador = 1

;;caar para fila
;;cadar para columna
;;Define los pesos de cada candidato
(define (FuncionObjetivo listaCandidatos matriz listaPesos max)
  (cond
    ((null? listaCandidatos) (reverse listaPesos))
    ;;Esquina Superior izquierda
    ((and (equal? (caar listaCandidatos) 1) (equal? (cadar listaCandidatos) 1)) (FuncionObjetivo (cdr listaCandidatos) matriz (append (list (+
                                                                                         (Peso (cadar listaCandidatos) (car (moverMatriz matriz (+ (caar listaCandidatos) 1) )) 1)
                                                                                         (Peso (+ (cadar listaCandidatos) 1) (car (moverMatriz matriz (+ (caar listaCandidatos) 1) )) 1)
                                                                                         (Peso (+ (cadar listaCandidatos) 1) (car (moverMatriz matriz (caar listaCandidatos))) 1))) listaPesos) max)) ;;Esquina Superior Izquierda
    
    ((and (equal? (caar listaCandidatos) 1) (equal? (cadar listaCandidatos) max)) (FuncionObjetivo (cdr listaCandidatos) matriz (append (list (+
                                                                                         (Peso (cadar listaCandidatos) (car (moverMatriz matriz (+ (caar listaCandidatos) 1) )) 1)
                                                                                         (Peso (- (cadar listaCandidatos) 1) (car (moverMatriz matriz (+ (caar listaCandidatos) 1) )) 1)
                                                                                         (Peso (- (cadar listaCandidatos) 1) (car (moverMatriz matriz (caar listaCandidatos))) 1))) listaPesos) max)) ;;Esquina Superior Derecha
    
    ((and (equal? (caar listaCandidatos) max) (equal? (cadar listaCandidatos) 1)) (FuncionObjetivo (cdr listaCandidatos) matriz (append (list (+
                                                                                         (Peso (cadar listaCandidatos) (car (moverMatriz matriz (- (caar listaCandidatos) 1) )) 1)
                                                                                         (Peso (+ (cadar listaCandidatos) 1) (car (moverMatriz matriz (- (caar listaCandidatos) 1) )) 1)
                                                                                         (Peso (+ (cadar listaCandidatos) 1) (car (moverMatriz matriz (caar listaCandidatos))) 1))) listaPesos) max)) ;;Ezquina Inferior izquierda
    
    ((and (equal? (caar listaCandidatos) max) (equal? (cadar listaCandidatos) max)) (FuncionObjetivo (cdr listaCandidatos) matriz (append (list (+
                                                                                         (Peso (cadar listaCandidatos) (car (moverMatriz matriz (- (caar listaCandidatos) 1) )) 1)
                                                                                         (Peso (- (cadar listaCandidatos) 1) (car (moverMatriz matriz (- (caar listaCandidatos) 1) )) 1)
                                                                                         (Peso (- (cadar listaCandidatos) 1) (car (moverMatriz matriz (caar listaCandidatos))) 1))) listaPesos) max)) ;;Ezquina inferior Derecha
    ((equal? (caar listaCandidatos) 1) (FuncionObjetivo (cdr listaCandidatos) matriz (append (list (+
                                                                                                    (Peso (- (cadar listaCandidatos) 1) (car (moverMatriz matriz (caar listaCandidatos))) 1)
                                                                                                    (Peso (- (cadar listaCandidatos) 1) (car (moverMatriz matriz (+ (caar listaCandidatos) 1))) 1)
                                                                                                    (Peso (cadar listaCandidatos) (car (moverMatriz matriz (+ (caar listaCandidatos) 1))) 1)
                                                                                                    (Peso (+ (cadar listaCandidatos) 1) (car (moverMatriz matriz (+ (caar listaCandidatos) 1))) 1)
                                                                                                    (Peso (+ (cadar listaCandidatos) 1) (car (moverMatriz matriz (caar listaCandidatos))) 1))) listaPesos) max)) ;;Primera Fila
                                                                                                                                                                                                                                                                                   
    ((equal? (caar listaCandidatos) max) (FuncionObjetivo (cdr listaCandidatos) matriz (append (list (+
                                                                                                    (Peso (- (cadar listaCandidatos) 1) (car (moverMatriz matriz (- (caar listaCandidatos) 1))) 1)
                                                                                                    (Peso (- (cadar listaCandidatos) 1) (car (moverMatriz matriz (caar listaCandidatos))) 1)
                                                                                                    (Peso (+ (cadar listaCandidatos) 1) (car (moverMatriz matriz (caar listaCandidatos))) 1)
                                                                                                    (Peso (+ (cadar listaCandidatos) 1) (car (moverMatriz matriz (- (caar listaCandidatos) 1))) 1) )) listaPesos) max)) ;;Ultima Fila
                                                                                                        
    ((equal? (cadar listaCandidatos) 1) (FuncionObjetivo (cdr listaCandidatos) matriz (append (list (+
                                                                                                     (Peso (cadar listaCandidatos) (car (moverMatriz matriz (- (caar listaCandidatos) 1))) 1)
                                                                                                     (Peso (+ (cadar listaCandidatos) 1) (car (moverMatriz matriz (- (caar listaCandidatos) 1))) 1)
                                                                                                     (Peso (+ (cadar listaCandidatos) 1) (car (moverMatriz matriz (caar listaCandidatos))) 1)
                                                                                                     (Peso (+ (cadar listaCandidatos) 1) (car (moverMatriz matriz (+ (caar listaCandidatos) 1))) 1)
                                                                                                     (Peso (cadar listaCandidatos) (car (moverMatriz matriz (+ (caar listaCandidatos) 1))) 1))) listaPesos) max)) ;;Primera columna

                                                                                                    
    ((equal? (cadar listaCandidatos) max) (FuncionObjetivo (cdr listaCandidatos) matriz (append (list (+
                                                                                                     (Peso (cadar listaCandidatos) (car (moverMatriz matriz (- (caar listaCandidatos) 1))) 1)
                                                                                                     (Peso (- (cadar listaCandidatos) 1) (car (moverMatriz matriz (- (caar listaCandidatos) 1))) 1)
                                                                                                     (Peso (- (cadar listaCandidatos) 1) (car (moverMatriz matriz (caar listaCandidatos))) 1)
                                                                                                     (Peso (- (cadar listaCandidatos) 1) (car (moverMatriz matriz (+ (caar listaCandidatos) 1))) 1)
                                                                                                     (Peso (cadar listaCandidatos) (car (moverMatriz matriz (+ (caar listaCandidatos) 1))) 1))) listaPesos) max)) ;;Ultima columna

    
    (else (FuncionObjetivo (cdr listaCandidatos) matriz (append (list (+
                                                                       (Peso (cadar listaCandidatos) (car (moverMatriz matriz (+ (caar listaCandidatos) 1))) 1)
                                                                       (Peso (cadar listaCandidatos) (car (moverMatriz matriz (- (caar listaCandidatos) 1))) 1)
                                                                       (Peso (- (cadar listaCandidatos) 1) (car (moverMatriz matriz (caar listaCandidatos))) 1)
                                                                       (Peso (+ (cadar listaCandidatos) 1) (car (moverMatriz matriz (caar listaCandidatos))) 1)
                                                                       (Peso (- (cadar listaCandidatos) 1) (car (moverMatriz matriz (+ (caar listaCandidatos) 1))) 1)
                                                                       (Peso (- (cadar listaCandidatos) 1) (car (moverMatriz matriz (- (caar listaCandidatos) 1))) 1)
                                                                       (Peso (+ (cadar listaCandidatos) 1) (car (moverMatriz matriz (+ (caar listaCandidatos) 1))) 1)
                                                                       (Peso (+ (cadar listaCandidatos) 1) (car (moverMatriz matriz (- (caar listaCandidatos) 1))) 1))) listaPesos) max))))
                                                                       
(define (Peso columna lista contColumna)
  (cond
    ((equal? columna contColumna) (CalculoPeso 0 (car lista)))
    (else (Peso columna (cdr lista) (+ 1 contColumna)))))

;;Función que retorna la matriz con una candidad de filas exactas
(define (moverMatriz matriz fila)
  (cond
    ((equal? fila 1) matriz)
    (else (moverMatriz (cdr matriz) (- fila 1)))))


(define (CalculoPeso PesoTotal PesoDado)
  (cond
    ((equal? PesoDado 0) (+ PesoTotal 0))
    ((equal? PesoDado 1) (+ PesoTotal 1))
    ((equal? PesoDado 2) (+ PesoTotal 2))))

(FuncionObjetivo '((5 1) (7 2) (5 3) (4 4) (5 5) (4 6) (3 7) (3 8)) matriz '() 8)

;;(define (Funcion_Viabilidad)) ;;Analiza si el candidato seleccionado sirve para obtener una solución

;;(define (Funcion_Seleccion)) ;;Selecciona el mejor candidato



;;(define (Funcion_Solucion)) ;;ANALIZA cuando cualquiera de los dos gana

