#lang racket

(require racket/include)
(include "enteros.rkt")
(define vacia (lambda (x) x))

(define construir (lambda (x)
    (lambda (y)
        ((par false) ((par x) y)))))

(define vacia? (lambda (l)
    (primero l)))

(define cabeza (lambda (l)
    (primero (segundo l))))

(define cola (lambda (l)
    (segundo (segundo l))))

(define lista-0 vacia)

(define lista-1 ((construir uno) vacia))

(define lista-2 ((construir uno) ((construir dos) vacia)))

(define lista-3 ((construir uno) ((construir dos) ((construir tres) ((construir cinco) ((construir ocho) vacia))))))


;hay que implementar longitud
(define comprobar-lista (lambda (l)
    (if (= (comprobar (longitud l)) 0)
        '()
        (cons (testenteros (cabeza l)) (comprobar-lista (cola l))))))

(define longitud
    (lambda (l)
        ((Y (lambda (f)
            (lambda(x)
                (((vacia? x)
                    (lambda (no_use)
                        zero
                        )
                    (lambda (no_use)
                        ((sumnat un) (f (cola x)))
                        )
                    )
                zero) ;pasa zero como argumento de no_use
                )
            ))
        l) ;Pasa l como el valor inicial de x
        )
    )

;test de pertenencia_________________________________________________________________
(define miembro?
  (lambda (mem)
    (lambda (lista)
      (((Y (lambda (f)
             (lambda (m)
               (lambda (l)
                 (((vacia? l)
                   ;fin de la lista, no se ha encontrado
                  (lambda (no_use) false)
                  (lambda(no_use)
                    ((((esigualent (cabeza l))m)
                     ;se ha encontrado
                     (lambda (no_use) true)
                     ;devolver miembro? m cola
                     (lambda (no_use)
                       ((f m) (cola l))
                       )
                     )zero)
                    )
                  )zero))
               )
             )
           )mem)
       lista)
      )
    )
  )
;unit test:
(quote false)
((miembro? dos) lista-0)
(quote true)
((miembro? dos) lista-2)
(quote false)
((miembro? dos) lista-1)
;Hacer el reverse de cada lista_______________________________________________

;Coger el último elemento de una lista
(define last
    (lambda (l)
        ((Y (lambda (f)
            (lambda(x)
                (((vacia? (cola x))
                    (lambda (no_use)
                        (cabeza x)
                        )
                    (lambda (no_use)
                        (f (cola x))
                        )
                    )
                zero) 
                )
            ))
        l) 
        )
    )
    
; Coger todos los elementos 
(define without-last
    (lambda (l)
        ((Y (lambda (f)
            (lambda(x)
                (((vacia? (cola x))
                    (lambda (no_use)
                        vacia
                        )
                    (lambda (no_use)
                        ((construir (cabeza x)) (f (cola x)))
                        )
                    )
                zero) 
                )
            ))
        l) 
        )
    )

;reverso de una lista
(define reverse
    (lambda (l)
        ((Y (lambda (f)
            (lambda(x)
                (((vacia? x)
                    (lambda (no_use)
                        vacia
                        )
                    (lambda (no_use)
                        ((construir (last x)) (f (without-last x)))
                        )
                    )
                zero) 
                )
            ))
        l) 
        )
    )

;sumar los elementos de una lista_______________________________________________
(define suma-lista
  (lambda (lista)
    ((Y 
 (lambda (f)
   (lambda (l)
     (((vacia? l)
      (lambda (no_use) zero)
      (lambda (no_use) 
        ((sument (cabeza l)) (f (cola l)))))zero)
    )))lista)
    )
  )
;unit test
(quote 0)
(suma-lista lista-0)
(quote 1)
(testenteros (suma-lista lista-1))
(quote 3)
(testenteros (suma-lista lista-2))
;concatenar dos listas___________________________________________________________

(define concatenar
  (lambda (lista1)
    (lambda (lista2)
       (((Y
(lambda (f)
  (lambda (l1)
    (lambda(l2)
      (((vacia? l1)
       (lambda (no_use)
         (((vacia? l2)
          (lambda (no_use) vacia)
          (lambda (no_use)
            ((construir
           (cabeza l2)) ((f l1) (cola l2)))
            )) zero)
         )
       (lambda (no_use)
         ((construir (cabeza l1)) ((f (cola l1)) l2))
         )) zero) ;zero -> no_use
      )))) lista1) lista2)
      )
    )
  )
;llamada recursiva
;unit test
(comprobar-lista ((concatenar lista-3) lista-3))



;sumar dos listas (como vectores)____________________________________________________
;definición
(define sumar_listas
  (lambda (lista1)
    (lambda (lista2)
       (((Y
(lambda (f)
  (lambda (l1)
    (lambda(l2)
      (((vacia? l1)
       (lambda (no_use) vacia)
       (lambda (no_use)
         (((vacia? l2)
          (lambda (no_use) vacia)
          (lambda (no_use)
            ((construir
           ((sument (cabeza l1)) (cabeza l2))) ((f (cola l1)) (cola l2)))
            )) zero)
         )) zero) ;zero -> no_use
      )))) lista1) lista2)
      )
    )
  )
;llamada recursiva
;unit test
(list 2)
(comprobar-lista ((sumar_listas lista-1) lista-1))
(list 2 4)
(comprobar-lista ((sumar_listas lista-2) lista-2))
(define lista-n ((construir tres) ((construir cinco) vacia)))
(list 4 7)
(comprobar-lista ((sumar_listas lista-2) lista-n))
