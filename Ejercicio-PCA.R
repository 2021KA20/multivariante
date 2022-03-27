#######################################
# ANÁLISIS DE COMPONENTES PRINCIPALES #
#######################################

# Ejercicio 1:
# Realiza los siguientes pasos, recuerda agregar tus notas
# personales al script.

# 1.- Instalacion del paquete datos
# 2.- Abrir la librería
# 3.- Elige una matriz
# 4.- Exploracion de la matriz
# 5.- Configuración y/o filtrado de variables (me quedo con las cuantitativas)
# 6.- Desarrollar el PCA paso a paso
# 7.- Construir el primer y segundo componente con las variables originales
# 8.- Realizar la interpretacion del resultado.
# 9.- Subir el script a tu repositorio en github.


# 1.- Instalacion del paquete datos:
install.packages("datos")

# 2.- Abrir la librería:
library(datos)

# 3.- Elige una matriz:
#Se selecciona la matriz flores y se guarda en una variable, en este caso será x.
x <- datos::flores

# 4.- Exploracion de la matriz:
#1.- Dimensión de la matriz
dim(x)

#La matriz cuenta con 150 observaciones y 5 variables.

#2.- Tipo de variables
str(x)

#3.- Nombre de las variables
colnames(x)

#4.- Saber si hay datos perdidos
anyNA(x)

#Para esta matriz no hay datos perdidos. 

# 5.- Configuración y/o filtrado de variables (me quedo con las cuantitativas):
#Tratamiento de la matriz:
#Se genera una nueva matriz *x1* que filtrará las variables 
#cuantitativas de la especie Versicolor.
x1 <- x[51:100,1:4]


# 6.- Desarrollar el PCA paso a paso:
#1.- Se transforma la matriz en un data.frame 
x1 <- as.data.frame(x1)

#2.- Definir n(individuos) y p(variables)
n <- dim(x1)[1]
p <- dim(x1)[2]

#3.- Generación de un scatterplot de las variables originales, 
#sin tomar en cuenta la variable cualitativa (Especie).
pairs(x1,col="red", pch=19, 
      main="Variables originales")

#4.- Obtención de la media por columna y la **matriz de covarianza muestral**.
mu <-colMeans(x1)
mu

s <-cov(x1)
s

#5.- Obtención de los **valores** y **vectores propios** desde la 
#matriz de covarianza muestral:
es <- eigen(s)
es

#5.1.- Separación de la matriz de valores propios.
eigen.val <-es$values
eigen.val

#5.2.- Separación de la matriz de vectores propios.
eigen.vec <-es$vectors
eigen.vec

#6.- Proporción de variabilidad para cada valor:
#6.1.- Para la matriz de valores propios.
pro.var<-eigen.val/sum(eigen.val)
pro.var

#6.2.- Proporción de variabilidad acumulada.
pro.var.acum<-cumsum(eigen.val)/sum(eigen.val)
pro.var.acum

#7.- Obtención de la matriz de correlaciones:
R<-cor(x1)
R

#8.- Obtención de los *valores* y *vectores propios* a partir de la
#**matriz de correlaciones**. 
eR<-eigen(R)
eR

#9.- Separación de la matriz de valores y vectores propios:
#9.1.- Separación de la matriz de valores propios.
eigen.val.R<-eR$values
eigen.val.R

#9.2.- Separación de la matriz de vectores propios.
eigen.vec.R<-eR$vectors
eigen.vec.R

#10.- Cálculo de la proporción de variabilidad: 
#10.1.- Para la matriz de valores propios.
pro.var.R<-eigen.val/sum(eigen.val)
pro.var.R

#10.2.- Proporción de variabilidad acumulada.
#En este punto, se selecciona el número de componentes siguiendo 
#el criterio del 80% de la varianza explicada.
pro.var.acum.R<-cumsum(eigen.val)/sum(eigen.val)
pro.var.acum.R

#En este caso, se seleccionan dos factores (0.896% de varianza explicada)

#11.- Cálculo de la media de los valores propios.
mean(eigen.val.R)

## Obtención de coeficientes
#12.- Centrar los datos con respecto a la media:
#12.1.- Construcción de matriz de 1
ones<-matrix(rep(1,n),nrow=n, ncol=1)

#12.2.- Construcción de la matriz centrada.
X.cen<-as.matrix(x1)-ones%*%mu

#13.- Construcción de la matriz diagonal de las covrianzas.
Dx<-diag(diag(s))
Dx

#14.-  Construcción de la matriz centrada multiplicada por Dx^1/2.
Y<-X.cen%*%solve(Dx)^(1/2)

#15.- Construccion de los coeficientes o scores eigen.vec.R matriz de autovectores.
scores<-Y%*%eigen.vec.R
scores

#16.- Nombramos las columnas PC1...PC4.
colnames(scores)<-c("PC1","PC2","PC3","PC4")

#17.- Visualización de los scores.
scores[1:10,]

#18.- Generacion del gráfico de los scores.
pairs(scores, main="scores", col="green", pch=10)



# ANÁLISIS DE COMPONENTES PRINCIPALES VÍA SINTETIZADA #

#1.- Cálculo de la varianza a las columnas:  1 = filas, 2 = columnas.
apply(x1, 2, var)

#2.- Aplicación de la función **prcomp** para reducir la dimensionalidad 
#y centrado por la media y escalada por la desviacion standar (dividir entre sd).
acp<-prcomp(x1, center=TRUE, scale=TRUE)
acp

#3.- Generación del gráfico **screeplot**
plot(acp, type="l", main = "Componentes principales")

#4.- Visualización del resumen de la matriz **ACP**
summary(acp)

# 7.- Construir el primer y segundo componente con las variables originales
# y 8.- Realizar la interpretacion del resultado.

# Combinación lineal de las variables originales.

#1.- Elaboración del primer componente principal:
  
  #z1 = -0.482(Largo.Sepalo) - 0.464(Ancho.Sepalo) - 0.534(Largo.Petalo) - 0.515(Ancho.Petalo)

#Este componente distingue entre flores grandes y pequeñas.

# Sépalo corto
# Sépalo angosto
# Pétalo corto
# Pétalo angosto


#2.- Elaboración del segundo componente principal:
  
  #z2 = -0.610(Largo.Sepalo) + 0.672(Ancho.Sepalo) - 0.306(Largo.Petalo) + 0.283(Ancho.Petalo)

#Este componente distingue flores por especie.

# Sépalo corto
# Sépalo ancho
# Pétalo corto
# Pétalo ancho
