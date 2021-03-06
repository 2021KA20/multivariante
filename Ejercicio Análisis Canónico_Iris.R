################################  
### AN�LISIS CAN�NICO - IRIS ###
################################

## Importar la matriz de datos
library(MASS)
iris<-as.data.frame(iris)

## Exploraci�n de la matriz
#1.- Dimensi�n de la matriz:
dim(iris)

#La base de datos cuenta con 150 observaciones y 5 variables.

#2.- Nombres de las columnas:
colnames(iris)

#3.- Tipo de variables:
str(iris)

#4.- Saber si existen datos nulos:
anyNA(iris)
#Esta base de datos no contiene datos nulos.

# Escalamiento de la matriz
#Paqueter�as a utilizar
library(tidyverse)

#1.- Generaci�n de variables X
X <- iris %>% 
  select(Sepal.Length, Sepal.Width) %>%
  scale()
head(X)

#2.- Generaci�n de variables Y
Y <- iris %>%
  select(Petal.Length,Petal.Width) %>%
  scale()
head(Y)

## An�lisis can�nico con un par de variables
#Librer�a y paqueter�a a utilizar
library(CCA)

#1.- An�lisis
ac<-cancor(X,Y)
ac

#2.- Visualizaci�n de la matriz X
ac$xcoef

#3.- Visualizaci�n de la matriz Y
ac$ycoef

#4.- Visualizaci�n de la correlaci�n can�nica
ac$cor

#5.- Obtenci�n de la matriz de variables can�nicas

#Se obtiene multiplicando los coeficientes por cada una de las variables (X1 y Y1)
ac1_X <- as.matrix(X) %*% ac$xcoef[, 1]
ac1_Y <- as.matrix(Y) %*% ac$ycoef[, 1]

#6.- Visualizaci�n de los primeros 20 datos
ac1_X[1:20,]
ac1_Y[1:20,]

#7.- Correlaci�n can�nica entre variable X1 y Y1
cor(ac1_X,ac1_Y)

#8.- Verificaci�n de la correlaci�n can�nica
assertthat::are_equal(ac$cor[1], 
                      cor(ac1_X,ac1_Y)[1])

## An�lisis can�nico con dos pares de variables
#1.- C�lculo de las variables X2 y Y2
ac2_X <- as.matrix(X) %*% ac$xcoef[, 2]
ac2_Y <- as.matrix(Y) %*% ac$ycoef[, 2]

#2.- Agregamos las variables generadas a la matriz original de penguins
ac_df <- iris %>% 
  mutate(ac1_X=ac1_X,
         ac1_Y=ac1_Y,
         ac2_X=ac2_X,
         ac2_Y=ac2_Y)

#3.- Visualizaci�n de los nombres de las variables
colnames(ac_df)

#4.- Generaci�n del grafico scater plot para la visualizaci�n de X1 y Y1
ac_df %>% 
  ggplot(aes(x=ac1_X,y=ac1_Y))+
  geom_point(color="indianred1") +
  xlab("Longitud del S�palo (can�nica)") +
  ylab("Longitud del P�talo (can�nica)") +
  ggtitle("Scater plot de X1 y Y1 can�nicos")

#5.- Generaci�n de un boxplot
ac_df %>% 
  ggplot(aes(x=Species,y=ac1_X, color=Species))+
  geom_boxplot(width=0.5)+
  geom_jitter(width=0.15)+
  ggtitle("Variable Can�nica X1 contra Especie")

#Se observa una correlacion entre la variable can�nica X1 y la variable latente Especie.

ac_df %>% 
  ggplot(aes(x=Species,y=ac1_Y, color=Species))+
  geom_boxplot(width=0.5)+
  geom_jitter(width=0.15)+
  ggtitle("Variable Can�nica Y1 contra Especie")

#6.- Construcci�n de un scater plot con las variables X1 y Y1, separados por especie
ac_df %>% 
  ggplot(aes(x=ac1_X,y=ac1_Y, color=Species))+
  geom_point()+
  ggtitle("Variable Can�nica X1 contra Y1")

#7.- Scater plot con las variables can�nicas X2 y Y2 separadas por g�nero.
ac_df %>% 
  ggplot(aes(x=ac2_X,y=ac2_Y, color=Species))+
  geom_point()+
  ggtitle("Variable Can�nica X2 contra Y2")

#No se identifica correlaci�n entre el conjunto de variables X2 y Y2 separadas por g�nero.

#8.- Generaci�n de la ecuaci�n can�nica
ac$xcoef
ac$ycoef

#9.- Sustituci�n en la ecuaci�n can�nica general
#**U1** = -0.072(Sepal.Length) + 0.030 (Sepal.Width)

#**V1** = -0.122(Petal.Length) + 0.043(Petal.Width)