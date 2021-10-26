Prueba 1 - Javier Ramos
================

## Carga de datos y librerías

Se cargará la data y las librerías necesarias para el posterior
análisis.

``` r
#Dataframe
df = read.table(file = "data.txt", header = TRUE)

#Librerías
library(mapproj)
```

    ## Loading required package: maps

## Pregunta A

### Desarrollo:

``` r
#Radio de la tierra
r = 6371

#Separar las columnas de longitud y latitud
x = df[,1]
y = df[,2]

#Se procede a realizar la proyección sinusoidal
prj_sinusoidal = mapproject(x, y, projection="sinusoidal",orientation=c(90,0,0)) 
result = r*cbind(prj_sinusoidal$x,prj_sinusoidal$y)
df2 = cbind(result, df[,3])

#Se renombran las filas y las columnas para facilitar la lectura de la tabla
colnames(df2) <- c("lon", "lat", "anom")
rownames(df2) <- 1:nrow(df2)
#df2
```

## Pregunta B

### Desarrollo:

## Pregunta C

### Desarrollo:

## Pregunta D

### Desarrollo:

## Pregunta E

### Desarrollo:

## Pregunta F

### Desarrollo:

## Pregunta G

### Desarrollo:

## Pregunta H

### Desarrollo:

## Pregunta I

### Desarrollo: