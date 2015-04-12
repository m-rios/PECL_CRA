%Reglas gramaticales

oracion --> g_nominal, g_verbal.

%definicion temporal
nexo --> [y].

g_nominal --> nombre.
g_nominal --> determinante, nombre.
g_nominal --> nombre, adjetivo.
g_nominal --> determinante, nombre, adjetivo.

g_p --> preposicion, g_nominal.

g_verbal --> verbo.
g_verbal --> verbo, g_nominal.
g_verbal --> verbo, adjetivo.
g_verbal --> verbo, g_p.

%Diccionario
determinante --> [el].
determinante --> [la].
determinante --> [un].
determinante --> [una].

preposicion --> [a].
preposicion --> [en].

nombre --> [hombre].
nombre --> [mujer].
nombre --> [juan].
nombre --> [maria].
nombre --> [manzana].
nombre --> [manzanas].
nombre --> [gato].
nombre --> [raton].
nombre --> [ratones].
nombre --> [alumno].
nombre --> [universidad].

verbo --> [ama].
verbo --> [come].
verbo --> [estudia].
verbo --> [cazo].
verbo --> [era].
verbo -->[es].

adjetivo --> [roja].
adjetivo --> [negro].
adjetivo --> [grande].
adjetivo --> [gris].
adjetivo --> [pequeno].
