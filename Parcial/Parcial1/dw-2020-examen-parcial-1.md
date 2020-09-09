dw-2020-parcial-1
================
Tepi
9/3/2020

# Examen parcial

Indicaciones generales:

  - Usted tiene el período de la clase para resolver el examen parcial.

  - La entrega del parcial, al igual que las tareas, es por medio de su
    cuenta de github, pegando el link en el portal de MiU.

  - Pueden hacer uso del material del curso e internet (stackoverflow,
    etc.). Sin embargo, si encontramos algún indicio de copia, se
    anulará el exámen para los estudiantes involucrados. Por lo tanto,
    aconsejamos no compartir las agregaciones que generen.

## Sección I: Preguntas teóricas.

  - Existen 10 preguntas directas en este Rmarkdown, de las cuales usted
    deberá responder 5. Las 5 a responder estarán determinadas por un
    muestreo aleatorio basado en su número de carné.

  - Ingrese su número de carné en `set.seed()` y corra el chunk de R
    para determinar cuáles preguntas debe responder.

<!-- end list -->

``` r
set.seed(20170480) 
v<- 1:10
preguntas <-sort(sample(v, size = 5, replace = FALSE ))

paste0("Mis preguntas a resolver son: ",paste0(preguntas,collapse = ", "))
```

    ## [1] "Mis preguntas a resolver son: 1, 6, 7, 9, 10"

### Listado de preguntas teóricas

1.  Para las siguientes sentencias de `base R`, liste su contraparte de
    `dplyr`:
    
      - `str()`
      - `df[,c("a","b")]`
      - `names(df)[4] <- "new_name"` donde la posición 4 corresponde a
        la variable `old_name`
      - `df[df$variable == "valor",]`

2.  Al momento de filtrar en SQL, ¿cuál keyword cumple las mismas
    funciones que el keyword `OR` para filtrar uno o más elementos una
    misma columna?

3.  ¿Por qué en R utilizamos funciones de la familia apply
    (lapply,vapply) en lugar de utilizar ciclos?

4.  ¿Cuál es la diferencia entre utilizar `==` y `=` en R?

5.  ¿Cuál es la forma correcta de cargar un archivo de texto donde el
    delimitador es `:`?

6.  ¿Qué es un vector y en qué se diferencia en una lista en R?

7.  ¿Qué pasa si quiero agregar una nueva categoría a un factor que no
    se encuentra en los niveles existentes?

8.  Si en un dataframe, a una variable de tipo `factor` le agrego un
    nuevo elemento que *no se encuentra en los niveles existentes*,
    ¿cuál sería el resultado esperado y por qué?
    
      - El nuevo elemento
      - `NA`

9.  En SQL, ¿para qué utilizamos el keyword `HAVING`?

10. Si quiero obtener como resultado las filas de la tabla A que no se
    encuentran en la tabla B, ¿cómo debería de completar la siguiente
    sentencia de SQL?
    
      - SELECT \* FROM A \_\_\_\_\_\_\_ B ON A.KEY = B.KEY WHERE
        \_\_\_\_\_\_\_\_\_\_ = \_\_\_\_\_\_\_\_\_\_

Extra: ¿Cuántos posibles exámenes de 5 preguntas se pueden realizar
utilizando como banco las diez acá presentadas? (responder con código de
R.)

## Sección II Preguntas prácticas.

  - Conteste las siguientes preguntas utilizando sus conocimientos de R.
    Adjunte el código que utilizó para llegar a sus conclusiones en un
    chunk del markdown.

A. De los clientes que están en más de un país,¿cuál cree que es el más
rentable y por qué?

B. Estrategia de negocio ha decidido que ya no operará en aquellos
territorios cuyas pérdidas sean “considerables”. Bajo su criterio,
¿cuáles son estos territorios y por qué ya no debemos operar ahí?

### Primera pregunta práctica

Los clientes más importantes con presencia internacional son `a17a7558`
y `ff122c3f`. Ambos encabezan el número de ventas siendo el top 3:

a17a7558 con $19817.70  
ff122c3f con $15358.54  
c53868a0 con $13812.87

No puedo dar una respuesta muy directa debido a que no conozco las
relaciones comerciales-personales que tenemos con estos clientes, no
obstante, por las ventas, me orientaría por `a17a7558`.

    ## `summarise()` regrouping output by 'Cliente' (override with `.groups` argument)

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 7 x 3
    ##   Cliente  ventas_totales conteo
    ##   <chr>             <dbl>  <int>
    ## 1 a17a7558         19818.      2
    ## 2 ff122c3f         15359.      2
    ## 3 c53868a0         13813.      2
    ## 4 044118d4          9436.      2
    ## 5 f676043b          3635.      2
    ## 6 f2aab44e           400.      2
    ## 7 bf1e94e9             0       2

### Segunda pregunta práctica

El territorio con más pérdidas son `f7dfc635`, con 14985.02 en pérdidas.
No obstante, no considero retirarnos de ese territorio, puesto que las
pérdidas representan un total muy pequeño de las ventas.

Haciendo una relación de pérdidas contra ventas totales en general, las
regiones de las cuales me retiraría son `68de9759`, `8682908b`,
`45c0376d` porque sus pérdidas representan una porción considerable con
respecto a sus ventas.

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 104 x 4
    ##    Territorio ventas_total perdidas razon_perdidas
    ##    <chr>             <dbl>    <dbl>          <dbl>
    ##  1 68de9759          3704.   -226.         -0.0609
    ##  2 8682908b          4612.   -245.         -0.0531
    ##  3 45c0376d         10646.   -475.         -0.0446
    ##  4 0320288f           845.    -33.6        -0.0397
    ##  5 4814799f         24263.   -664.         -0.0274
    ##  6 5a464f3f          4795.   -126.         -0.0263
    ##  7 0bbe6418          8892.   -218.         -0.0245
    ##  8 d43e8f6a          5711.   -136.         -0.0238
    ##  9 6eff1266          7440.   -176.         -0.0237
    ## 10 c31adb2f          9883.   -230.         -0.0233
    ## # ... with 94 more rows

### I. Preguntas teóricas

## Primera pregunta (1)

``` r
###resuelva acá
```

Respondiendo en orden a los puntos:

  - No conozco una función que haga exactamente lo mismo. Yo me las
    arreglaría usando `summarise` y `aggregate`. Ahora si quiero mostrar
    los datos usaría `View()`.

  - Usaría un select

<!-- end list -->

``` 
 select(df, a, b)
```

  - Usaría la opción de rename

<!-- end list -->

    rename(df, old_name = new_name)

  - Usaría un filtro.

<!-- end list -->

    filter(df, variable == "valor")

## Segunda pregunta (4) {Me salió la vez cuando el parámetro estaba en 6}

No es lo mismo `==` y `=`. `==` se usa para comparaciones, pongo como
ejemplo la función filter. `=` se usa para asignar valores y tiene como
equivalente `<-`.

## Segunda pregunta (6)

Respondiendo de manera sobria y directa: Una Lista puede contener datos
con distintos tipos como: Numeric, Character, logical, etc. Un Vector es
similar, pero todos los elementos son del mismo tipo.

## Tercera pregunta (7)

Yo lo agregaría de la siguiente manera. El único problema sería que no
tendría mucho sentido a no ser de que se vaya a usar la nueva etiqueta.

`levels(df$column) <- c(levels(df$column), newFactorLevel)`

## Cuarta pregunta (9)

Utilizamos el keyword `HAVING` para agregar una cláusula a un `GROUP
BY`. Utilizo como ejemplo:

    SELECT COUNT(CustomerID), Country
    FROM Customers
    GROUP BY Country
    HAVING COUNT(CustomerID) > 5

Se hace una agrupación por país, con el conteo de clientes, pero en vez
de usar la cláusula `WHERE`se utiliza `HAVING` para indicar que sólo
queremos los datos cuyo conteo es mayor que 5.

## Quinta pregunta (10)

La pregunta es algo extraña. Yo lo que haría sería:

> Con LEFT obtengo todo lo de A, como usé \* me permite obtener todos
> los registros y sólo filtro por los vacíos.

`SELECT * FROM A LEFT JOIN B ON A.KEY = B.KEY WHERE B.KEY IS NULL`

## EXTRA (E)

Serían 210. Usando comninaciones, donde se cuentan los mismos elementos
sin importar el orden.

``` r
choose(10, 6)
```

    ## [1] 210
