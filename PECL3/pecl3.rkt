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

(define lista-4 ((construir uno) ((construir ocho) ((construir cinco) ((construir cinco) ((construir nueve) vacia))))))

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


;máximo de una lista_________________________________________________________________
(define maximo
    (lambda (lista)
      ((Y (lambda (f)
               (lambda (l)
                 (((vacia?  l)
                  (lambda (no_use)  zero)
                  (lambda(no_use)
                    (((vacia? (cola l))
                      (lambda (no_use) (cabeza l))
                      (lambda(no_use)
                        ((((esmenoroigualent (cabeza l)) (cabeza (cola l)))
                          (lambda (no_use) (f (cola l)))
                          (lambda (no_use)
                       (f ((construir (cabeza l)) (cola (cola l))))
                       )
                     )zero)
                    )
                  )zero)
                    )
                  )zero))
             )
           )
       lista)
      )
  )
;unit test:
(testenteros (maximo lista-2))

;minimo de una lista_________________________________________________________________
(define minimo
    (lambda (lista)
      ((Y (lambda (f)
               (lambda (l)
                 (((vacia?  l)
                  (lambda (no_use)  zero)
                  (lambda(no_use)
                    (((vacia? (cola l))
                      (lambda (no_use) (cabeza l))
                      (lambda(no_use)
                        ((((esmayoroigualent (cabeza l)) (cabeza (cola l)))
                          (lambda (no_use) (f (cola l)))
                          (lambda (no_use)
                       (f ((construir (cabeza l)) (cola (cola l))))
                       )
                     )zero)
                    )
                  )zero)
                    )
                  )zero))
             )
           )
       lista)
      )
  )

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
;unit test
(comprobar-lista ((concatenar lista-3) lista-3))



;sumar dos listas (como vectores)____________________________________________________
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
;unit test
(list 2)
(comprobar-lista ((sumar_listas lista-1) lista-1))
(list 2 4)
(comprobar-lista ((sumar_listas lista-2) lista-2))
(define lista-n ((construir tres) ((construir cinco) vacia)))
(list 4 7)
(comprobar-lista ((sumar_listas lista-2) lista-n))
;inverso lista________________________________________________________________________
(define inversa
  (lambda (lista)
    ((Y
      (lambda (f)
        (lambda (l)
          (((vacia? l)
            (lambda (no_use) vacia)
            (lambda (no_use)
              ((concatenar (f (cola l))) ((construir (cabeza l))vacia))
              )
            ) zero)
          )
        )
      ) lista)
    )
  )
(list 8 5 3 2 1)
(comprobar-lista (inversa lista-3))
(comprobar-lista (inversa lista-2))
(comprobar-lista (inversa lista-0))

;borrado en lista________________________________________________________________________
(define borrar
  (lambda (mem)
    (lambda (lista)
      (((Y (lambda (f)
             (lambda (m)
               (lambda (l)
                 (((vacia? l)
                  (lambda (no_use) vacia)
                  (lambda(no_use)
                    ((((esigualent (cabeza l))m)
                     (lambda (no_use) (cola l))
                     (lambda (no_use)
                       ((construir (cabeza l)) ((f m) (cola l)))
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
  
;Ordenación mayor a menor en lista________________________________________________________________________
(define mayoramenor
  (lambda (lista)
    ((Y 
 (lambda (f)
   (lambda (l)
     (((vacia? l)
      (lambda (no_use) vacia)
      (lambda (no_use) 
        ((construir (maximo l)) (f ((borrar (maximo l))l )) )))zero)
    )))lista)
    )
  )
  
;Unit test
(comprobar-lista (mayoramenor lista-4))

;Ordenación menor a mayor en lista________________________________________________________________________

(define menoramayor
  (lambda (lista)
    ((Y 
 (lambda (f)
   (lambda (l)
     (((vacia? l)
      (lambda (no_use) vacia)
      (lambda (no_use) 
        ((construir (minimo l)) (f ((borrar (minimo l))l )) )))zero)
    )))lista)
    )
  )

;Unit test
(comprobar-lista (menoramayor lista-4))

