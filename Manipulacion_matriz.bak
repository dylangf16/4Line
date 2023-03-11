#lang scheme

(provide 4inLine)

;funcion que construye una lista de 0s del tamano dado
(define (buildList list0 m)
  (cond ((equal? m 0)
         list0)
        (else
         (buildList (append list0 '(0)) (- m 1))
         )))

;funcion que construye una matriz de 0s de tamano n x m
(define (buildMatrix matrix n m)
  (cond ((equal? n 0)
         matrix)
        (else
         (buildMatrix (append matrix (list (buildList '() m))) (- n 1) m)
         )))

;funcion que reemplaza un valor en una posicion de la lista
(define (replaceInList list0 pos value count)
  (cond ((null? list0)
         null)
        ((equal? pos count)
         (append (list value) (cdr list0)))
        (else
         (append (list (car list0)) (replaceInList (cdr list0) pos value (+ count 1)))
         )))

;reemplaza un valor en la posicion n x m en una matriz
(define (replaceInMatrix matrix n m value count)
  (cond ((null? matrix)
         null)
        ((equal? n count)
         (append (list (replaceInList (car matrix) m value 0)) (cdr matrix)))
        (else
         (append (list (car matrix)) (replaceInMatrix (cdr matrix) n m value (+ count 1))))
        ))

;Encuentra si hay 4 elementos seguidos en una lista
(define (4inLine_aux list0 team count)
  (cond ((equal? count 4)
         #t)
        ((null? list0)
         #f)
        ((equal? (car list0) team)
         (4inLine_aux (cdr list0) team (+ count 1)))
        (else
         (4inLine_aux (cdr list0) team 0))))

;Encuentra si hay 4 elementos horizontales seguidos iguales en una lista
(define (4inLine_horizontal matrix team)
  (cond ((null? matrix)
         #f)
        ((4inLine_aux (car matrix) team 0)
         #t)
        (else
         (4inLine_horizontal (cdr matrix) team))))

;Obtiene el elemento que se encuentra en la posicion dada en una lista
(define (getFromList list0 pos count)
  (cond ((null? list0)
         null)
        ((equal? pos count)
         (car list0))
        (else
         (getFromList (cdr list0) pos (+ count 1)))))

;Obtiene el elemento que se encuentra en la posicion n x m de una matriz
(define (getFromMatrix matrix n m count)
  (cond ((null? matrix)
         null)
        ((equal? n count)
         (getFromList (car matrix) m 0))
        (else
         (getFromMatrix (cdr matrix) n m (+ count 1))
        )))

;Convierte una columna de una matriz en una lista
(define (getColumn matrix column)
  (cond ((null? matrix)
         null)
        (else
         (append (list (getFromList (car matrix) column 0)) (getColumn (cdr matrix) column)))))

;Encuentra si hay 4 elementos iguales seguidos verticalmente
(define (4inLine_vertical matrix team count)
  (cond ((null? (getFromList (car matrix) count 0))
         #f)
        ((4inLine_aux (getColumn matrix count) team 0)
         #t)
        (else
         (4inLine_vertical matrix team (+ count 1)))))

;funcion que obtiene la cantidad de filas de una matriz
(define (matrixLenght matrix count)
  (cond ((null? matrix)
         count)
        (else
         (matrixLenght (cdr matrix) (+ count 1)))))

;funcion para obtener una diagonal de una matriz en forma de lista
(define (getDiagonal matrix startN startM)
  (cond ((null? (getFromMatrix matrix startN startM 0))
        null)
        (else
         (append (list (getFromMatrix matrix startN startM 0)) (getDiagonal matrix (+ startN 1) (+ startM 1))))))

;funcion para retorna las diagonales decrecientes inferiores o medias, mayores o iguales a 4
(define (getBottomDecreasedDiagonal matrix startPoint)
  (cond ((equal? startPoint (- (matrixLenght matrix 0) 3))
         null)
        (else
         (append (list (getDiagonal matrix startPoint 0)) (getBottomDecreasedDiagonal matrix (+ startPoint 1))))))

;funcion para retorna las diagonales decrecientes inferiores o medias, mayores o iguales a 4
(define (getTopDecreasedDiagonal matrix startPoint)
  (cond ((equal? startPoint (- (matrixLenght matrix 0) 3))
         null)
        (else
         (append (list (getDiagonal matrix 0 (+ startPoint 1))) (getTopDecreasedDiagonal matrix (+ startPoint 1))))))

;funcion que obtiene todas las diagonales decrecientes de una lista
(define (getDecreasedDiagonal matrix)
  (cond ((null? matrix)
         null)
        (else
         (append (reverse (getTopDecreasedDiagonal matrix 0)) (getBottomDecreasedDiagonal matrix 0)))))

;funcion que retorna todas las diagonales de una lista
(define (getAllDiagonals matrix)
  (cond ((null? matrix)
         null)
        (else
         (append (getDecreasedDiagonal matrix) (getDecreasedDiagonal (getReverseMatrix matrix))))))

;funcion que retorna la matriz en reversa horizontalmente
(define (getReverseMatrix matrix)
  (cond ((null? matrix)
         null)
        (else
         (append (list (reverse (car matrix))) (getReverseMatrix (cdr matrix))))))

;funcion que verfica si hay 4 en linea diagonal
(define (4inLine_diagonal matrix team)
  (cond ((null? matrix)
         null)
        (else
         (4inLine_diagonal_aux (getAllDiagonals matrix) team))))

;funcion auxiliar que verifica si alguna diagonal tiene 4 elementos seguidos
(define (4inLine_diagonal_aux diagonalsList team)
  (cond ((null? diagonalsList)
         #f)
        ((4inLine_aux (car diagonalsList) team 0)
         #t)
        (else
         (4inLine_diagonal_aux (cdr diagonalsList) team))))

;funcion que verifica si hay 4 en linea en cualquier posicion en la matriz         
(define (4inLine matrix team)
  (cond ((4inLine_horizontal matrix team)
         #t)
        ((4inLine_vertical matrix team 0)
         #t)
        ((4inLine_diagonal matrix team)
         #t)
        (else
         #f)))

