#lang scheme
(require "Manipulacion_matriz.rkt")


(define matriz '((0 0 0 0 0 0 0 0)
                 (0 0 0 0 0 0 0 0)
                 (0 0 0 0 0 0 0 0)
                 (0 0 0 0 0 0 1 2)
                 (0 0 0 1 0 2 2 1)
                 (1 0 1 2 1 2 1 2)
                 (2 0 2 2 1 1 2 1)
                 (1 2 1 2 1 1 2 2)))


;;SIEMPRE (fila columna)


;;Input: matriz, matriz (esta va a ser la que se va a manipular), '(), 1
;;La función lo que hace es ir línea por línea hasta chocar con números y ceros, si encuentra una fila con ambas condiciones y si encima no tiene un número
;;Agrega la posición de encima del número topado como posible candidato
;;Output: lista de candidatos, la posición de estos corresponde a la columna en la que están
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
    ((equal? (- fila 1) 0) (conjuntoCandidatosAUX (eliminaUnValor filaPorAnalizar '()) fila (cambiarValorEspecifico 0 0 listaCandidatos '() 1)))
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
  ((and (equal? fila 0) (equal? (car lista) 0)) (append listaRespuesta (list (list 0 0)) (cdr lista)))
  ((and (equal? columna contador) (equal? (car lista) 0)) (append listaRespuesta (list (list fila columna)) (cdr lista)))
  (else (cambiarValorEspecifico fila columna (cdr lista) (append listaRespuesta (list (car lista))) (+ contador 1)))))

(conjuntoCandidatos matriz matriz '(0 0 0 0 0 0 0 0) 1)

;;Notas para mover en lista:
;;caar para fila
;;cadar para columna
;;Define los pesos de cada candidato

;;Input: la lista de candidatos obtenida anteriormente, la matriz completa, '() y el número de columnas
;;La función lo que hace es verificar los posibles campos al rededor de la posición dada por listaCandidatos
;;Y dependiendo de las fichas que haya, da un peso, si la ficha alrededor es de jugador -> +8 // si  si la ficha alrededor es de ia -> +4
;;Output: lista con los pesos (al igual que listaCandidatos, su posición corresponde a la columna donde está el candidato)
;;Hay que agregar si la posición es (0,0), que de un peso de 0
(define (FuncionObjetivo listaCandidatos matriz listaPesos max)
  (cond
    ((null? listaCandidatos) (reverse listaPesos))
    ((and (equal? (caar listaCandidatos) 0) (equal? (cadar listaCandidatos)0)  (FuncionObjetivo (cdr listaCandidatos) matriz (append (list 0)) max)))
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
                                                                       (Peso (cadar listaCandidatos) (car (moverMatriz matriz (+ (caar listaCandidatos) 1))) 1) ;;El resto de posibles posiciones
                                                                       (Peso (cadar listaCandidatos) (car (moverMatriz matriz (- (caar listaCandidatos) 1))) 1)
                                                                       (Peso (- (cadar listaCandidatos) 1) (car (moverMatriz matriz (caar listaCandidatos))) 1)
                                                                       (Peso (+ (cadar listaCandidatos) 1) (car (moverMatriz matriz (caar listaCandidatos))) 1)
                                                                       (Peso (- (cadar listaCandidatos) 1) (car (moverMatriz matriz (+ (caar listaCandidatos) 1))) 1)
                                                                       (Peso (- (cadar listaCandidatos) 1) (car (moverMatriz matriz (- (caar listaCandidatos) 1))) 1)
                                                                       (Peso (+ (cadar listaCandidatos) 1) (car (moverMatriz matriz (+ (caar listaCandidatos) 1))) 1)
                                                                       (Peso (+ (cadar listaCandidatos) 1) (car (moverMatriz matriz (- (caar listaCandidatos) 1))) 1))) listaPesos) max))))
;;Funcion que retorna el valor de la posicion dada                                                                       
(define (Peso columna lista contColumna)
  (cond
    ((equal? columna contColumna) (CalculoPeso 0 (car lista)))
    (else (Peso columna (cdr lista) (+ 1 contColumna)))))

;;Función que retorna la matriz con una candidad de filas exactas inferiores
(define (moverMatriz matriz fila)
  (cond
    ((equal? fila 1) matriz)
    (else (moverMatriz (cdr matriz) (- fila 1)))))

;;Función que retorna la matriz con una candidad de filas exactas superiores
(define (moverMatrizSuperior matriz fila matrizNueva)
  (cond
    ((equal? fila 0) matrizNueva)
    (else (moverMatrizSuperior (cdr matriz) (- fila 1) (append matrizNueva (list (car matriz)))))))

;;Da un valor con respecto al valor dado en la funcion Peso
(define (CalculoPeso PesoTotal PesoDado)
  (cond
    ((equal? PesoDado 0) (+ PesoTotal 0))
    ((equal? PesoDado 1) (+ PesoTotal 8))
    ((equal? PesoDado 2) (+ PesoTotal 4))))

(FuncionObjetivo '((5 1) (7 2) (5 3) (4 4) (5 5) (4 6) (3 7) (3 8)) matriz '() 8)

;;Input: lista de pesos obtenida anteriormente, lista de candidatos obtenida anterioremente, matriz total, '()
;;Analiza si el candidato seleccionado sirve para obtener una solución
;;Output: lista actualizada de los pesos
(define (Funcion_Viabilidad listaPesos listaCandidatos matriz nuevaListaPesos)
  (cond
    ((null? listaCandidatos) nuevaListaPesos)
    ((equal? (4inLine
       (construirNuevaMatrizTemp (car (moverMatriz matriz (caar listaCandidatos))) (cadar listaCandidatos) '() (moverMatrizSuperior matriz (- (caar listaCandidatos) 1) '()) (moverMatriz matriz  (+ (caar listaCandidatos) 1)) '() 1) 2) #t)
     (Funcion_Viabilidad (cdr listaPesos) (cdr listaCandidatos) matriz (append nuevaListaPesos (list (+ (car listaPesos) 100)))))
    (else
     (Funcion_Viabilidad (cdr listaPesos) (cdr listaCandidatos) matriz (append nuevaListaPesos (list (car listaPesos)))))))

;;Funcion que construye una nueva Matriz temporal, para analizar si la ficha hace un 4 en linea
(define (construirNuevaMatrizTemp fila columna nuevaFila matrizSup matrizInf matrizNueva contador)
  (cond
    ((equal? columna contador) (append matrizNueva matrizSup (list  (append nuevaFila (list 2) (cdr fila))) matrizInf))
    (else (construirNuevaMatrizTemp (cdr fila) columna (append nuevaFila (list (car fila)) ) matrizSup matrizInf matrizNueva (+ contador 1)))))

(Funcion_Viabilidad '(1 10 4 1 8 5 3 3) '((5 1) (7 2) (5 3) (4 4) (5 5) (4 6) (3 7) (3 8)) matriz '())
;;Input:
;;La funcion lo que hace es retornar la posición del candidato con mayor peso
;;Output: posición donde va a caer la ficha
(define (FuncionSeleccion listaCandidatos listaPesosFinales contador posMayor pesoMayor)
  (cond
    ((null? listaPesosFinales) (FuncionSeleccionAux listaCandidatos posMayor))
    ((> (car listaPesosFinales) pesoMayor) (FuncionSeleccion listaCandidatos (cdr listaPesosFinales) 1 (+ posMayor contador) (car listaPesosFinales)))
    (else (FuncionSeleccion listaCandidatos (cdr listaPesosFinales) (+ contador 1) posMayor pesoMayor))))

(define (FuncionSeleccionAux listaCandidatos mayor)
  (cond
    ((equal? mayor 1) (car listaCandidatos))
    (else (FuncionSeleccionAux (cdr listaCandidatos) (- mayor 1)))))

(FuncionSeleccion '((5 1) (7 2) (5 3) (4 4) (5 5) (4 6) (3 7) (3 8)) '(1 110 4 1 108 5 3 3) 1 0 0)


