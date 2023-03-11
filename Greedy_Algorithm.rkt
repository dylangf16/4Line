#lang scheme
(require "Manipulacion_matriz.rkt")
(require "Miscelaneos.rkt")
(provide conjuntoCandidatos)

;;Conjunto Candidatos -> línea 118
;;Funcion Seleccion -> línea 108
;;Función Viabilidad -> línea 24
;;Función Objetivo -> línea 44
;;Función solución -> En "Manipulación_matriz.rkt" línea 162 


(define matrix '((0 0 0 0 0 0 0 0)
                 (0 0 0 0 0 0 0 0)
                 (0 0 0 0 0 0 0 0)
                 (0 0 0 0 0 0 1 2)
                 (0 0 0 1 0 2 2 2)
                 (1 0 1 2 1 2 1 2)
                 (2 0 2 2 1 1 2 1)
                 (1 2 1 2 1 1 2 2)))

(define matrix2 '((0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0)
                  (1 0 0 0 0 0 0 0)))

;;Input: lista de pesos obtenida anteriormente, lista de candidatos obtenida anterioremente, matriz total, '()
;;Analiza si el candidato seleccionado sirve para obtener una solución
;;Output: lista actualizada de los pesos
(define (Funcion_Viabilidad listaPesos listaCandidatos matriz nuevaListaPesos)
  (cond
    ((null? listaCandidatos) (funcion_viabilidadAUX listaPesos listaCandidatos matriz nuevaListaPesos))
    ((equal? (4inLine
       (construirNuevaMatrizTemp (car (moverMatriz matriz (caar listaCandidatos))) (cadar listaCandidatos) '() (moverMatrizSuperior matriz (- (caar listaCandidatos) 1) '()) (moverMatriz matriz  (+ (caar listaCandidatos) 1)) '() 1) 2) #t)
     (Funcion_Viabilidad (cdr listaPesos) (cdr listaCandidatos) matriz (append nuevaListaPesos (list (+ (car listaPesos) 100)))))
    (else
     (Funcion_Viabilidad (cdr listaPesos) (cdr listaCandidatos) matriz (append nuevaListaPesos (list (car listaPesos)))))))


(define (funcion_viabilidadAUX listaPesos listaCandidatos matriz nuevaListaPesos)
  (cond
    ((null? listaCandidatos) (display nuevaListaPesos) nuevaListaPesos)
    ((equal? (4inLine
       (construirNuevaMatrizTemp (car (moverMatriz matriz (caar listaCandidatos))) (cadar listaCandidatos) '() (moverMatrizSuperior matriz (- (caar listaCandidatos) 1) '()) (moverMatriz matriz  (+ (caar listaCandidatos) 1)) '() 1) 1) #t)
     (funcion_viabilidadAUX (cdr listaPesos) (cdr listaCandidatos) matriz (append nuevaListaPesos (list (+ (car listaPesos) 50)))))
    (else
     (funcion_viabilidadAUX (cdr listaPesos) (cdr listaCandidatos) matriz (append nuevaListaPesos (list (car listaPesos)))))))

;;Notas para mover en lista:
;;caar para fila
;;cadar para columna
;;Define los pesos de cada candidato

;;Input: la lista de candidatos obtenida anteriormente, la matriz completa, '() y el número de columnas
;;La función lo que hace es verificar los posibles campos al rededor de la posición dada por listaCandidatos
;;Y dependiendo de las fichas que haya, da un peso, si la ficha alrededor es de jugador -> +8 // si  si la ficha alrededor es de ia -> +4
;;Output: lista con los pesos (al igual que listaCandidatos, su posición corresponde a la columna donde está el candidato)
;;Hay que agregar si la posición es (0,0), que de un peso de 0
(define (FuncionObjetivo listaCandidatos listaCandidatosSinAlterar matriz listaPesos maxFilas maxColumnas)
  (cond
    ((null? listaCandidatos) (Funcion_Viabilidad (reverse listaPesos) listaCandidatosSinAlterar matriz '()))
    ((and (equal? (caar listaCandidatos) 0) (equal? (cadar listaCandidatos)0)  (FuncionObjetivo (cdr listaCandidatos) listaCandidatosSinAlterar matriz (append (list 0) listaPesos) maxFilas maxColumnas)))
    ((and (equal? (caar listaCandidatos) 1) (equal? (cadar listaCandidatos) 1)) (FuncionObjetivo (cdr listaCandidatos) listaCandidatosSinAlterar matriz (append (list (+
                                                                                         (Peso (cadar listaCandidatos) (car (moverMatriz matriz (+ (caar listaCandidatos) 1) )) 1)
                                                                                         (Peso (+ (cadar listaCandidatos) 1) (car (moverMatriz matriz (+ (caar listaCandidatos) 1) )) 1)
                                                                                         (Peso (+ (cadar listaCandidatos) 1) (car (moverMatriz matriz (caar listaCandidatos))) 1))) listaPesos) maxFilas maxColumnas)) ;;Esquina Superior Izquierda
    
    ((and (equal? (caar listaCandidatos) 1) (equal? (cadar listaCandidatos) maxColumnas)) (FuncionObjetivo (cdr listaCandidatos)listaCandidatosSinAlterar matriz (append (list (+
                                                                                         (Peso (cadar listaCandidatos) (car (moverMatriz matriz (+ (caar listaCandidatos) 1) )) 1)
                                                                                         (Peso (- (cadar listaCandidatos) 1) (car (moverMatriz matriz (+ (caar listaCandidatos) 1) )) 1)
                                                                                         (Peso (- (cadar listaCandidatos) 1) (car (moverMatriz matriz (caar listaCandidatos))) 1))) listaPesos) maxFilas maxColumnas)) ;;Esquina Superior Derecha
    
    ((and (equal? (caar listaCandidatos) maxFilas) (equal? (cadar listaCandidatos) 1)) (FuncionObjetivo (cdr listaCandidatos) listaCandidatosSinAlterar matriz (append (list (+
                                                                                         (Peso (cadar listaCandidatos) (car (moverMatriz matriz (- (caar listaCandidatos) 1) )) 1)
                                                                                         (Peso (+ (cadar listaCandidatos) 1) (car (moverMatriz matriz (- (caar listaCandidatos) 1) )) 1)
                                                                                         (Peso (+ (cadar listaCandidatos) 1) (car (moverMatriz matriz (caar listaCandidatos))) 1))) listaPesos) maxFilas maxColumnas)) ;;Ezquina Inferior izquierda
    
    ((and (equal? (caar listaCandidatos) maxFilas) (equal? (cadar listaCandidatos) maxColumnas)) (FuncionObjetivo (cdr listaCandidatos) listaCandidatosSinAlterar matriz (append (list (+
                                                                                         (Peso (cadar listaCandidatos) (car (moverMatriz matriz (- (caar listaCandidatos) 1) )) 1)
                                                                                         (Peso (- (cadar listaCandidatos) 1) (car (moverMatriz matriz (- (caar listaCandidatos) 1) )) 1)
                                                                                         (Peso (- (cadar listaCandidatos) 1) (car (moverMatriz matriz (caar listaCandidatos))) 1))) listaPesos) maxFilas maxColumnas)) ;;Ezquina inferior Derecha
    ((equal? (caar listaCandidatos) 1) (FuncionObjetivo (cdr listaCandidatos) listaCandidatosSinAlterar matriz (append (list (+
                                                                                                    (Peso (- (cadar listaCandidatos) 1) (car (moverMatriz matriz (caar listaCandidatos))) 1)
                                                                                                    (Peso (- (cadar listaCandidatos) 1) (car (moverMatriz matriz (+ (caar listaCandidatos) 1))) 1)
                                                                                                    (Peso (cadar listaCandidatos) (car (moverMatriz matriz (+ (caar listaCandidatos) 1))) 1)
                                                                                                    (Peso (+ (cadar listaCandidatos) 1) (car (moverMatriz matriz (+ (caar listaCandidatos) 1))) 1)
                                                                                                    (Peso (+ (cadar listaCandidatos) 1) (car (moverMatriz matriz (caar listaCandidatos))) 1))) listaPesos) maxFilas maxColumnas)) ;;Primera Fila
                                                                                                                                                                                                                                                                                   
    ((equal? (caar listaCandidatos) maxFilas) (FuncionObjetivo (cdr listaCandidatos) listaCandidatosSinAlterar matriz (append (list (+
                                                                                                    (Peso (- (cadar listaCandidatos) 1) (car (moverMatriz matriz (- (caar listaCandidatos) 1))) 1)
                                                                                                    (Peso (- (cadar listaCandidatos) 1) (car (moverMatriz matriz (caar listaCandidatos))) 1)
                                                                                                    (Peso (+ (cadar listaCandidatos) 1) (car (moverMatriz matriz (caar listaCandidatos))) 1)
                                                                                                    (Peso (+ (cadar listaCandidatos) 1) (car (moverMatriz matriz (- (caar listaCandidatos) 1))) 1) )) listaPesos) maxFilas maxColumnas)) ;;Ultima Fila
                                                                                                        
    ((equal? (cadar listaCandidatos) 1) (FuncionObjetivo (cdr listaCandidatos) listaCandidatosSinAlterar matriz (append (list (+
                                                                                                     (Peso (cadar listaCandidatos) (car (moverMatriz matriz (- (caar listaCandidatos) 1))) 1)
                                                                                                     (Peso (+ (cadar listaCandidatos) 1) (car (moverMatriz matriz (- (caar listaCandidatos) 1))) 1)
                                                                                                     (Peso (+ (cadar listaCandidatos) 1) (car (moverMatriz matriz (caar listaCandidatos))) 1)
                                                                                                     (Peso (+ (cadar listaCandidatos) 1) (car (moverMatriz matriz (+ (caar listaCandidatos) 1))) 1)
                                                                                                     (Peso (cadar listaCandidatos) (car (moverMatriz matriz (+ (caar listaCandidatos) 1))) 1))) listaPesos) maxFilas maxColumnas)) ;;Primera columna

                                                                                                    
    ((equal? (cadar listaCandidatos) maxFilas) (FuncionObjetivo (cdr listaCandidatos) listaCandidatosSinAlterar matriz (append (list (+
                                                                                                     (Peso (cadar listaCandidatos) (car (moverMatriz matriz (- (caar listaCandidatos) 1))) 1)
                                                                                                     (Peso (- (cadar listaCandidatos) 1) (car (moverMatriz matriz (- (caar listaCandidatos) 1))) 1)
                                                                                                     (Peso (- (cadar listaCandidatos) 1) (car (moverMatriz matriz (caar listaCandidatos))) 1)
                                                                                                     (Peso (- (cadar listaCandidatos) 1) (car (moverMatriz matriz (+ (caar listaCandidatos) 1))) 1)
                                                                                                     (Peso (cadar listaCandidatos) (car (moverMatriz matriz (+ (caar listaCandidatos) 1))) 1))) listaPesos) maxFilas maxColumnas)) ;;Ultima columna

    
    (else (FuncionObjetivo (cdr listaCandidatos) listaCandidatosSinAlterar matriz (append (list (+
                                                                       (Peso (cadar listaCandidatos) (car (moverMatriz matriz (+ (caar listaCandidatos) 1))) 1) ;;El resto de posibles posiciones
                                                                       (Peso (cadar listaCandidatos) (car (moverMatriz matriz (- (caar listaCandidatos) 1))) 1)
                                                                       (Peso (- (cadar listaCandidatos) 1) (car (moverMatriz matriz (caar listaCandidatos))) 1)
                                                                       (Peso (+ (cadar listaCandidatos) 1) (car (moverMatriz matriz (caar listaCandidatos))) 1)
                                                                       (Peso (- (cadar listaCandidatos) 1) (car (moverMatriz matriz (+ (caar listaCandidatos) 1))) 1)
                                                                       (Peso (- (cadar listaCandidatos) 1) (car (moverMatriz matriz (- (caar listaCandidatos) 1))) 1)
                                                                       (Peso (+ (cadar listaCandidatos) 1) (car (moverMatriz matriz (+ (caar listaCandidatos) 1))) 1)
                                                                       (Peso (+ (cadar listaCandidatos) 1) (car (moverMatriz matriz (- (caar listaCandidatos) 1))) 1))) listaPesos) maxFilas maxColumnas))))


;;Input: Lista de Candidatos, lista de Pesos con Viabilidad, 1 0 0
;;La funcion lo que hace es retornar la posición del candidato con mayor peso
;;Output: posición donde va a caer la ficha
(define (FuncionSeleccion listaCandidatos listaPesosFinales contador posMayor pesoMayor)
  (cond
    ((null? listaPesosFinales) (FuncionSeleccionAux listaCandidatos posMayor))
    ((> (car listaPesosFinales) pesoMayor) (FuncionSeleccion listaCandidatos (cdr listaPesosFinales) 1 (+ posMayor contador) (car listaPesosFinales)))
    (else (FuncionSeleccion listaCandidatos (cdr listaPesosFinales) (+ contador 1) posMayor pesoMayor))))

;;SIEMPRE (fila columna)

;;Input: matriz, matriz (esta va a ser la que se va a manipular), '(), 1 maxFilas maxColumnas
;;La función lo que hace es ir línea por línea hasta chocar con números y ceros, si encuentra una fila con ambas condiciones y si encima no tiene un número
;;Agrega la posición de encima del número topado como posible candidato
;;Output: lista de candidatos, la posición de estos corresponde a la columna en la que están
(define (conjuntoCandidatos matriz matrizRecorrida listaCandidatos fila maxFilas maxColumnas)
  (cond
    ((null? matrizRecorrida) (FuncionSeleccion (replace-all listaCandidatos 1 maxFilas'()) (FuncionObjetivo (replace-all listaCandidatos 1 maxFilas'()) (replace-all listaCandidatos 1 maxFilas'()) matriz '() maxFilas maxColumnas) 1 0 0)) ;;salida
    
    ((equal? (verifCeros (car matrizRecorrida)) #t)
         (conjuntoCandidatos matriz (cdr matrizRecorrida) listaCandidatos (+ fila 1) maxFilas maxColumnas)) ;;verif si la fila está llena de 0
    
    ((equal? (verifNum (car matrizRecorrida)) #t)
     (conjuntoCandidatos matriz (cdr matrizRecorrida) (conjuntoCandidatosAUX (car matrizRecorrida) fila listaCandidatos maxFilas) (+ fila 1) maxFilas maxColumnas)) ;;verif si la fila está llena de números
    
    (else (conjuntoCandidatos matriz (cdr matrizRecorrida) (conjuntoCandidatosAUX (car matrizRecorrida) fila listaCandidatos maxFilas) (+ fila 1) maxFilas maxColumnas))))

(define (conjuntoCandidatosAUX filaPorAnalizar fila listaCandidatos maxFilas)
  (cond
    ((equal? (verifCeros filaPorAnalizar) #t) listaCandidatos)
    ((equal? (- fila 1) 0) (conjuntoCandidatosAUX (eliminaUnValor filaPorAnalizar '()) fila (cambiarValorEspecifico 0 0 listaCandidatos '() 1 maxFilas) maxFilas))
    (else (conjuntoCandidatosAUX (eliminaUnValor filaPorAnalizar '()) fila (cambiarValorEspecifico (- fila 1) (encontrarColumna filaPorAnalizar 0) listaCandidatos '() 1 maxFilas) maxFilas))))

(conjuntoCandidatos matrix2 matrix2 '(0 0 0 0 0 0 0 0) 1 8 8)



