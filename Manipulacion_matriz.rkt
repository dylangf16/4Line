#lang scheme

(provide 4inLine buildMatrix replaceInMatrix insertToken buildList)


(define matrix2 '((0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0)
                  (1 0 0 0 1 0 0 0)))

;----------------------------------------------------------

;;Input: lista vacia donde se va a ir construyendo recursivamente la lista de 0s, tamano de la lista
;;funcion auxiliar para construir la matriz de 0s, construye una lista de 0s del tamano dado
;;Output: lista del tamano dado de 0s
(define (buildList list0 m)
  (cond ((equal? m 0)
         list0)
        (else
         (buildList (append list0 '(0)) (- m 1))
         )))

;;Input: matriz vacia donde se va a ir construyendo recursivamente la matriz de 0s, cantidad de filas de la matriz, cantidad de columnas de la matriz
;;Construye una matriz de 0s del tamano n x m
;;Output: matriz de 0s del tamano dado
(define (buildMatrix matrix n m)
  (cond ((equal? n 0)
         matrix)
        (else
         (buildMatrix (append matrix (list (buildList '() m))) (- n 1) m)
         )))

;------------------------------------------------------------

;;Input: lista (fila de la matriz) donde se desea realizar el cambio de valor, posicion en la que esta el valor que se desea cambiar, valor por el que se va a remplazar, 0
;;Funcion auxiliar para sustituir en la matriz, sustituye en una lista la posicion dada por el valor correspondido
;;Output: lista con el valor reemplazado
(define (replaceInList list0 pos value count)
  (cond ((null? list0)
         null)
        ((equal? pos count)
         (append (list value) (cdr list0)))
        (else
         (append (list (car list0)) (replaceInList (cdr list0) pos value (+ count 1)))
         )))

;;Input: matriz donde se desea realizar el cambio de valor, fila donde se encuentra la posicion que se desea cambiar, columna donde esta la posicion que se desea cambiar, valor que se desea colocar en dicha posicion, 0
;;Sustituye en una matriz la posicion n x m por el valor dado
;;Output: matriz con el valor reemplazado
(define (replaceInMatrix matrix n m value count)
  (cond ((null? matrix)
         null)
        ((equal? n count)
         (append (list (replaceInList (car matrix) m value 0)) (cdr matrix)))
        (else
         (append (list (car matrix)) (replaceInMatrix (cdr matrix) n m value (+ count 1))))
        ))

;------------------------------------------------------------

;;Input: lista donde se desea hacer la verificacion, equipo que se desea verificar (1 o 2), 0
;;Funcion auxiliar para encontrar los elementos seguidos, en horizontal, vertical y diagnonal, encuentra si hay 4 elementos seguidos en una lista
;;Output: valor booleano, #t si hay 4 elementos seguidos y #f en caso contrario
(define (4inLine_aux list0 team count)
  (cond ((equal? count 4)
         #t)
        ((null? list0)
         #f)
        ((equal? (car list0) team)
         (4inLine_aux (cdr list0) team (+ count 1)))
        (else
         (4inLine_aux (cdr list0) team 0))))

;;Input: matriz de juego, equipo que se desea verificar (1 o 2)
;;Encuentra si hay 4 elementos horizontales seguidos iguales en una matriz
;;Output: valor booleano, #t si hay 4 elementos seguidos y #f en caso contrario
(define (4inLine_horizontal matrix team)
  (cond ((null? matrix)
         #f)
        ((4inLine_aux (car matrix) team 0)
         #t)
        (else
         (4inLine_horizontal (cdr matrix) team))))

;;Input: lista de donde se desea obtener el elemento, posicion de la lista donde se encuentra el elemento, 0
;;Obtiene el elemento que se encuentra en la posicion dada en una lista
;;Output: valor que se encuentra en la posicion de la lista
(define (getFromList list0 pos count)
  (cond ((null? list0)
         null)
        ((equal? pos count)
         (car list0))
        (else
         (getFromList (cdr list0) pos (+ count 1)))))

;;Input: matriz de donde se desea obtener el elemento, fila en la que esta el elemento, columna en la que esta el elemento, 0
;;Obtiene el elemento que se encuentra en la posicion n x m de una matriz
;;Output: valor que se encuentra en la posicion de la matriz
(define (getFromMatrix matrix n m count)
  (cond ((null? matrix)
         null)
        ((equal? n count)
         (getFromList (car matrix) m 0))
        (else
         (getFromMatrix (cdr matrix) n m (+ count 1))
        )))

;;Input: matriz de donde se desea obtener una columna, numero de columna que se desea obtener
;;Funcion auxiliar para encontrar 4 en linea vertical, convierte una columna de una matriz en una lista
;;Output: columna de una matriz en forma de lista
(define (getColumn matrix column)
  (cond ((null? matrix)
         null)
        (else
         (append (list (getFromList (car matrix) column 0)) (getColumn (cdr matrix) column)))))

;;Input: matriz de juego, equipo que se desea verificar los 4 en linea vertical (1 o 2), 0
;;Encuentra si hay 4 elementos iguales seguidos verticalmente
;;Output: valor booleano correspondiente a si hay o no 4 en linea vertical
(define (4inLine_vertical matrix team count)
  (cond ((null? (getFromList (car matrix) count 0))
         #f)
        ((4inLine_aux (getColumn matrix count) team 0)
         #t)
        (else
         (4inLine_vertical matrix team (+ count 1)))))

;;Input: matriz de juego, 0
;;funcion auxiliar para otras auxiliares de 4 en linea diagonal, obtiene la cantidad de filas de una matriz
;;Output: cantidad de filas de la matriz
(define (matrixLenght matrix count)
  (cond ((null? matrix)
         count)
        (else
         (matrixLenght (cdr matrix) (+ count 1)))))

;;Input: matriz de juego, fila en la que se desea iniciar la diagonal (0), columna donde se desea iniciar la diagonal
;;funcion para obtener una diagonal de una matriz en forma de lista
;;Output: diagonal de una matriz en forma de lista
(define (getDiagonal matrix startN startM)
  (cond ((null? (getFromMatrix matrix startN startM 0))
        null)
        (else
         (append (list (getFromMatrix matrix startN startM 0)) (getDiagonal matrix (+ startN 1) (+ startM 1))))))

;;Input: matriz de juego, punto n x n de donde se desea iniciar la diagonal de la matriz
;;funcion auxiliar para obtener todas las diagnoales de la matriz, obtiene las diagonales decrecientes inferiores o medias, con tamano mayor o igual a 4
;;Output: diagonal de la mitad de la matriz y todas las menores, en forma de lista
(define (getBottomDecreasedDiagonal matrix startPoint)
  (cond ((equal? startPoint (- (matrixLenght matrix 0) 3))
         null)
        (else
         (append (list (getDiagonal matrix startPoint 0)) (getBottomDecreasedDiagonal matrix (+ startPoint 1))))))

;;Input: matriz de juego, punto n x n de donde se desea iniciar la diagonal de la matriz
;;funcion auxiliar para obtener todas las diagnoales de la matriz, obtiene las diagonales decrecientes superiores, con un tamano mayor o igual a 4
;;Output: diagonales decrecientes mayores a la mitad en forma de lista
(define (getTopDecreasedDiagonal matrix startPoint)
  (cond ((equal? startPoint (- (matrixLenght matrix 0) 3))
         null)
        (else
         (append (list (getDiagonal matrix 0 (+ startPoint 1))) (getTopDecreasedDiagonal matrix (+ startPoint 1))))))

;;Input: matriz de juego
;;Funcion que obtiene todas las diagonales, inferiores, superiores y la media, de una matriz
;;Output: matriz que contiene todas las diagonales decrecientes, en forma de lista, de la matriz de juego
(define (getDecreasedDiagonal matrix)
  (cond ((null? matrix)
         null)
        (else
         (append (reverse (getTopDecreasedDiagonal matrix 0)) (getBottomDecreasedDiagonal matrix 0)))))

;;Input: matriz de juego
;;funcion auxiliar para obtener las diagonales crecientes de una matriz, le da vuelta horizontalmente a una matriz
;;Output: matriz inversa de la matriz de juego
(define (getReverseMatrix matrix)
  (cond ((null? matrix)
         null)
        (else
         (append (list (reverse (car matrix))) (getReverseMatrix (cdr matrix))))))

;;Input: matriz de juego
;;Obtiene todas las diagonales, decrecientes y crecientes, de una matriz
;;Output: matriz que contiene todas las diagonales, en forma de lista, de la matriz de juego
(define (getAllDiagonals matrix)
  (cond ((null? matrix)
         null)
        (else
         (append (getDecreasedDiagonal matrix) (getDecreasedDiagonal (getReverseMatrix matrix))))))

;;Input: matriz de diagonales de la matriz de juego, equipo del que se desea hacer la verificacion (1 o 2)
;;Funcion auxiliar que verifica si alguna diagonal tiene 4 elementos seguidos
;;Output: valor booleano, #t en caso de haber 4 en linea diagonal, #f de otra forma
(define (4inLine_diagonal_aux diagonalsList team)
  (cond ((null? diagonalsList)
         #f)
        ((4inLine_aux (car diagonalsList) team 0)
         #t)
        (else
         (4inLine_diagonal_aux (cdr diagonalsList) team))))

;;Input: matriz de juego, equipo del que se desea hacer la verificacion (1 o 2)
;;funcion que verfica si hay 4 en linea diagonal
;;Output: valor booleano, #t en caso de haber 4 en linea diagonal, #f de otra forma
(define (4inLine_diagonal matrix team)
  (cond ((null? matrix)
         null)
        (else
         (4inLine_diagonal_aux (getAllDiagonals matrix) team))))

;;Input: matriz de juego, equipo del que se desea hacer la verificacion (1 o 2)
;;Funcion que verifica si hay 4 en linea en cualquier posicion en la matriz  
;;Output: valor booleano, #t en caso de haber 4 en linea, #f en caso de no haber       
(define (4inLine matrix team)
  (cond ((4inLine_horizontal matrix team)
         #t)
        ((4inLine_vertical matrix team 0)
         #t)
        ((4inLine_diagonal matrix team)
         #t)
        (else
         #f)))

;--------------------------------------------


;;Input: columna de la matriz de juego donde se desea insertar la ficha, 0
;;funcion auxiliar de insertar Token, busca en que espacio de la columna puede caer la nueva ficha
;;retorna el valor n (fila de la matriz) donde puede ir la nueva ficha
(define (insertTokenAux column count)
  (cond
    ((null? column) 0)
    ((not (equal? (car column) 0))
         (- count 1))
        (else
         (insertTokenAux (cdr column) (+ count 1)))))
         
;;Input: matriz de juego, columna donde se desea insertar la ficha
;;Cambia el primer valor vacio (0), de la columna dada de la matriz de juego, por el valor del jugador (1)
;;Output: matriz de juego con el la nueva ficha insertada.
(define (insertToken matrix column)
  (cond ((null? matrix)
         null)
        (else
         (replaceInMatrix matrix (insertTokenAux (getColumn matrix column) 0) column 1 0))))


(insertToken matrix2 0)
