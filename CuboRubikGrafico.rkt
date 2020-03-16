#lang racket
(provide run)
(require (lib "graphics.ss" "graphics"))(open-graphics) ;;incluye la libreria que se va a utilizar

;;Tamano de las filas del cubo
(define tamano 4)
;;creacion de ventana con ancho y largo
(define ventana null)
(define ventana2 null);;ventana auxiliar en la que se dibuja

;;Se definen los valores del cubo a dibujar
(define listaCuadros null)

(define (run lista size)
  (set! listaCuadros lista)
  (set! tamano size)
  (set! ventana(open-viewport "Cubo Rubik" (* tamano 100) (* tamano 100)))
  (set! ventana2 (open-pixmap "ventana auxiliar" 800 600));;ventana auxiliar en la que se dibuja
  (teclado #\a)
  )



;;busca la lista de la cara que se quiere dibujar
(define (buscarCara indice lista)
  (cond ((null? lista)
         #f)
        ((equal? indice 1)
         (car lista)
         )
        (else
         (buscarCara (- indice 1)(cdr lista)))))

;;busca el color de un cuadro en una lista segun su indice
(define (elemento indice lista)
  (cond ((null? lista)
         #f)
        ((equal? indice 1)
         (~a(car lista))
         )
        (else
         (elemento (- indice 1)(cdr lista)))))

;;dibuja una cara en la ventana
(define (dibujarCara cara contador)
  (begin
    (for ([i (in-range tamano)])
      (for([j (in-range tamano)])
        ((draw-solid-rectangle ventana2) (make-posn (* 100 j) (* 100 i)) 100 100 (elemento contador (buscarCara cara listaCuadros)))
        (set! contador (+ 1 contador))
        )
      )
    ;;dibuja lineas para distinguir los cuadros de la cara
    (for ([h (in-range (+ 1 tamano))])
      (for([k (in-range (+ 1 tamano))])
        ((draw-solid-rectangle ventana2) (make-posn (* 100 k) (* 100 h)) 5 (* 100 tamano) "black")
        ((draw-solid-rectangle ventana2) (make-posn (* 100 k) (* 100 h)) (* 100 tamano) 5 "black")
        )
      )
    (copy-viewport ventana2 ventana);;copia el dibujo que hay en la ventana auxiliar a la ventana principal
    ((clear-viewport ventana2));;limpia la ventana auxiliar
    )
  )

;;controles
(define (teclado tecla)
  (if (equal? tecla #\w)
      (begin ;;llama a varias funciones a la vez
        (dibujarCara 5 1)
        (teclado (key-value (get-key-press ventana))))
      
      (if (equal? tecla #\s)
          (begin
            (dibujarCara 2 1)
            (teclado (key-value (get-key-press ventana))))
      
          (if (equal? tecla #\d)
              (begin
                (dibujarCara 3 1)
                (teclado (key-value (get-key-press ventana))))
      
              (if (equal? tecla #\a)
                  (begin
                    (dibujarCara 1 1)
                    (teclado (key-value (get-key-press ventana))))
                  
                  (if (equal? tecla #\f)
                      (begin
                        (dibujarCara 4 1)
                        (teclado (key-value (get-key-press ventana))))
                      
                      (if (equal? tecla #\x)
                          (begin
                            (dibujarCara 6 1)
                            (teclado (key-value (get-key-press ventana))))
                  
                          (if (equal? tecla #\space)
                              (begin
                                (close-viewport ventana)
                                listaCuadros)
                             
                             
                              ;else
                              (teclado (key-value (get-key-press ventana)))
                              )
                          ) 
                      )
                  )
              )
          )
      )
  )