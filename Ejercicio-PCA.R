#######################################
# AN�LISIS DE COMPONENTES PRINCIPALES #
#######################################

# Ejercicio 1:
# Realiza los siguientes pasos, recuerda agregar tus notas
# personales al script.

# 1.- Instalacion del paquete datos
# 2.- Abrir la librer�a
# 3.- Elige una matriz
# 4.- Exploracion de la matriz
# 5.- Configuraci�n y/o filtrado de variables (me quedo con las cuantitativas)
# 6.- Desarrollar el PCA paso a paso
# 7.- Construir el primer y segundo componente con las variables originales
# 8.- Realizar la interpretacion del resultado.
# 9.- Subir el script a tu repositorio en github.


# 1.- Instalacion del paquete datos:
install.packages("datos")

# 2.- Abrir la librer�a:
library(datos)

# 3.- Elige una matriz:
#Se selecciona la matriz flores y se guarda en una variable, en este caso ser� x.
x <- datos::flores

# 4.- Exploracion de la matriz:
#1.- Dimensi�n de la matriz
dim(x)

#La matriz cuenta con 150 observaciones y 5 variables.

#2.- Tipo de variables
str(x)

#3.- Nombre de las variables
colnames(x)

#4.- Saber si hay datos perdidos
anyNA(x)

#Para esta matriz no hay datos perdidos. 

# 5.- Configuraci�n y/o filtrado de variables (me quedo con las cuantitativas):
#Tratamiento de la matriz:
#Se genera una nueva matriz *x1* que filtrar� las variables 
#cuantitativas de la especie Versicolor.
x1 <- x[51:100,1:4]


# 6.- Desarrollar el PCA paso a paso:
#1.- Se transforma la matriz en un data.frame 
x1 <- as.data.frame(x1)

#2.- Definir n(individuos) y p(variables)
n <- dim(x1)[1]
p <- dim(x1)[2]

#3.- Generaci�n de un scatterplot de las variables originales, 
#sin tomar en cuenta la variable cualitativa (Especie).
pairs(x1,col="red", pch=19, 
      main="Variables originales")

#4.- Obtenci�n de la media por columna y la **matriz de covarianza muestral**.
mu <-colMeans(x1)
mu

s <-cov(x1)
s

#5.- Obtenci�n de los **valores** y **vectores propios** desde la 
#matriz de covarianza muestral:
es <- eigen(s)
es

#5.1.- Separaci�n de la matriz de valores propios.
eigen.val <-es$values
eigen.val

#5.2.- Separaci�n de la matriz de vectores propios.
eigen.vec <-es$vectors
eigen.vec

#6.- Proporci�n de variabilidad para cada valor:
#6.1.- Para la matriz de valores propios.
pro.var<-eigen.val/sum(eigen.val)
pro.var

#6.2.- Proporci�n de variabilidad acumulada.
pro.var.acum<-cumsum(eigen.val)/sum(eigen.val)
pro.var.acum

#7.- Obtenci�n de la matriz de correlaciones:
R<-cor(x1)
R

#8.- Obtenci�n de los *valores* y *vectores propios* a partir de la
#**matriz de correlaciones**. 
eR<-eigen(R)
eR

#9.- Separaci�n de la matriz de valores y vectores propios:
#9.1.- Separaci�n de la matriz de valores propios.
eigen.val.R<-eR$values
eigen.val.R

#9.2.- Separaci�n de la matriz de vectores propios.
eigen.vec.R<-eR$vectors
eigen.vec.R

#10.- C�lculo de la proporci�n de variabilidad: 
#10.1.- Para la matriz de valores propios.
pro.var.R<-eigen.val/sum(eigen.val)
pro.var.R

#10.2.- Proporci�n de variabilidad acumulada.
#En este punto, se selecciona el n�mero de componentes siguiendo 
#el criterio del 80% de la varianza explicada.
pro.var.acum.R<-cumsum(eigen.val)/sum(eigen.val)
pro.var.acum.R

#En este caso, se seleccionan dos factores (0.896% de varianza explicada)

#11.- C�lculo de la media de los valores propios.
mean(eigen.val.R)

## Obtenci�n de coeficientes
#12.- Centrar los datos con respecto a la media:
#12.1.- Construcci�n de matriz de 1
ones<-matrix(rep(1,n),nrow=n, ncol=1)

#12.2.- Construcci�n de la matriz centrada.
X.cen<-as.matrix(x1)-ones%*%mu

#13.- Construcci�n de la matriz diagonal de las covrianzas.
Dx<-diag(diag(s))
Dx

#14.-  Construcci�n de la matriz centrada multiplicada por Dx^1/2.
Y<-X.cen%*%solve(Dx)^(1/2)

#15.- Construccion de los coeficientes o scores eigen.vec.R matriz de autovectores.
scores<-Y%*%eigen.vec.R
scores

#16.- Nombramos las columnas PC1...PC4.
colnames(scores)<-c("PC1","PC2","PC3","PC4")

#17.- Visualizaci�n de los scores.
scores[1:10,]

#18.- Generacion del gr�fico de los scores.
pairs(scores, main="scores", col="green", pch=10)



# AN�LISIS DE COMPONENTES PRINCIPALES V�A SINTETIZADA #

#1.- C�lculo de la varianza a las columnas:  1 = filas, 2 = columnas.
apply(x1, 2, var)

#2.- Aplicaci�n de la funci�n **prcomp** para reducir la dimensionalidad 
#y centrado por la media y escalada por la desviacion standar (dividir entre sd).
acp<-prcomp(x1, center=TRUE, scale=TRUE)
acp

#3.- Generaci�n del gr�fico **screeplot**
plot(acp, type="l", main = "Componentes principales")

#4.- Visualizaci�n del resumen de la matriz **ACP**
summary(acp)

# 7.- Construir el primer y segundo componente con las variables originales
# y 8.- Realizar la interpretacion del resultado.

# Combinaci�n lineal de las variables originales.

#1.- Elaboraci�n del primer componente principal:
  
  #z1 = -0.482(Largo.Sepalo) - 0.464(Ancho.Sepalo) - 0.534(Largo.Petalo) - 0.515(Ancho.Petalo)

#Este componente distingue entre flores grandes y peque�as.

# S�palo corto
# S�palo angosto
# P�talo corto
# P�talo angosto


#2.- Elaboraci�n del segundo componente principal:
  
  #z2 = -0.610(Largo.Sepalo) + 0.672(Ancho.Sepalo) - 0.306(Largo.Petalo) + 0.283(Ancho.Petalo)

#Este componente distingue flores por especie.

# S�palo corto
# S�palo ancho
# P�talo corto
# P�talo ancho
