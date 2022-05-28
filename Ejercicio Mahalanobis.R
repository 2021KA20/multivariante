
#_____ Distancia de Mahalanobis____

### CALVO 

# Cargar los datos
ventas= c( 1054, 1057, 1058, 1060, 1061, 1060, 1061, 
           1062, 1062, 1064, 1062, 1062, 1064, 1056, 
           1066, 1070)
clientes= c(63, 66, 68, 69, 68, 71, 70, 70, 71, 72, 72, 
            73, 73, 75, 76, 78)

# Utilizamos la funcion data.frame() para crear 
# un juego de datos en R
datos <- data.frame(ventas ,clientes)

dim(datos)
str(datos)
summary(datos)

#----------------------------------------------------
#    Calculo de la distancia
#----------------------------------------------------

# El metodo de distancia Mahalanobis mejora el 
# metodo clasico de distancia de Gauss 
# eliminando el efecto que pueden producir 
# la correlacion entre las variables a analizar


# Determinar el numero de outlier que queremos encontrar.
num.outliers <- 2

# Ordenar los datos de mayor a menor distancia, 
# segun la metrica de Mahalanobis.
mah.ordenacion <- order(mahalanobis(datos, colMeans(datos), cov(datos)), decreasing=TRUE)
mah.ordenacion

# Generar un vector boleano los dos 
# valores mas alejados segun la distancia Mahalanobis.
outlier2 <- rep(FALSE , nrow(datos))
outlier2[mah.ordenacion[1:num.outliers]] <- TRUE

# Resaltar con un punto relleno los 2 valores outliers.
colorear.outlier <- outlier2 *16

# Visualizar el grafico con los datos destacando sus outlier.
plot(datos , pch=0)
points(datos , pch=colorear.outlier)

#En el gráfico se identifican los outliers.

#----------------------------------
#   Ejercicio 2: Proporcionado por R
#----------------------------------

require(graphics)

ma <- cbind(1:6, 1:3)
(S <-  var(ma))
mahalanobis(c(0, 0), 1:2, S)

x <- matrix(rnorm(100*3), ncol = 3)
stopifnot(mahalanobis(x, 0, 
                      diag(ncol(x))) == rowSums(x*x))

##- Here, D^2 = usual squared Euclidean distances

Sx <- cov(x)
D2 <- mahalanobis(x, colMeans(x), Sx)
plot(density(D2, bw = 0.5),
     main="Squared Mahalanobis distances, 
     n=100, p=3") ; rug(D2)
qqplot(qchisq(ppoints(100), df = 3), D2,
       main = expression("Q-Q plot of Mahalanobis" * ~D^2 *
                           " vs. quantiles of" * ~ chi[3]^2))
abline(0, 1, col = 'gray')


#----------------
#   Ejercicio 3
#----------------

# Diseñar un ejercicio utilizando la distancia de
# Mahalanobis.

# Incluye:
# 1.- Planteamiento del problema. (Cuál es el problema que se va a resolver)
### Utilizando la matriz state.x77, se quiere saber la distancia de Mahalanobis
### de la población y su esperanza de vida. Qué expectativas de vida
### tiene la población de Estados Unidos.

# 2.- Simular los datos o utilizar una matriz
#     Precargada en R.
### Se utiliza la matriz precargada en R de state.x77

# 3.- Dar tu interpretacion.
# Nota: Una vez que terminaste subes el script a
# tu repositorio en GitHub. Si te sobra tiempo 
# puedes ir creando el pdf en markdown. 
      
# Cargar los datos
datos <- as.data.frame(state.x77)
datos <- datos[,cbind(1,4)]
dim(datos)
str(datos)
summary(datos)

#----------------------------------------------------
#    Calculo de la distancia
#----------------------------------------------------

# El metodo de distancia Mahalanobis mejora el 
# metodo clasico de distancia de Gauss 
# eliminando el efecto que pueden producir 
# la correlacion entre las variables a analizar


# Determinar el numero de outlier que queremos encontrar.
num.outliers <- 10

# Ordenar los datos de mayor a menor distancia, 
# segun la metrica de Mahalanobis.
mah.ordenacion <- order(mahalanobis(datos, colMeans(datos), cov(datos)), decreasing=TRUE)
mah.ordenacion

# Generar un vector boleano los dos 
# valores mas alejados segun la distancia Mahalanobis.
outlier2 <- rep(FALSE , nrow(datos))
outlier2[mah.ordenacion[1:num.outliers]] <- TRUE

# Resaltar con un punto relleno los 10 valores outliers.
colorear.outlier <- outlier2 *16

# Visualizar el grafico con los datos destacando sus outlier.
plot(datos , pch=0)
points(datos , pch=colorear.outlier)

#3.- Interpretación:
#En el gráfico se identifican los outliers. Las distancias de cada uno
#están algo lejanas, hay outliers que se separan del resto de los datos y otros
#que, a pesar de que están cerca de la población, mantienen cierta lejanía.
#Pero la mayoría de los datos se mantienen cerca.