################################  
### ANÁLISIS CANÓNICO - IRIS ###
################################

## Importar la matriz de datos
library(MASS)
iris<-as.data.frame(iris)

## Exploración de la matriz
#1.- Dimensión de la matriz:
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
#Paqueterías a utilizar
library(tidyverse)

#1.- Generación de variables X
X <- iris %>% 
  select(Sepal.Length, Sepal.Width) %>%
  scale()
head(X)

#2.- Generación de variables Y
Y <- iris %>%
  select(Petal.Length,Petal.Width) %>%
  scale()
head(Y)

## Análisis canónico con un par de variables
#Librería y paquetería a utilizar
library(CCA)

#1.- Análisis
ac<-cancor(X,Y)
ac

#2.- Visualización de la matriz X
ac$xcoef

#3.- Visualización de la matriz Y
ac$ycoef

#4.- Visualización de la correlación canónica
ac$cor

#5.- Obtención de la matriz de variables canónicas

#Se obtiene multiplicando los coeficientes por cada una de las variables (X1 y Y1)
ac1_X <- as.matrix(X) %*% ac$xcoef[, 1]
ac1_Y <- as.matrix(Y) %*% ac$ycoef[, 1]

#6.- Visualización de los primeros 20 datos
ac1_X[1:20,]
ac1_Y[1:20,]

#7.- Correlación canónica entre variable X1 y Y1
cor(ac1_X,ac1_Y)

#8.- Verificación de la correlación canónica
assertthat::are_equal(ac$cor[1], 
                      cor(ac1_X,ac1_Y)[1])

## Análisis canónico con dos pares de variables
#1.- Cálculo de las variables X2 y Y2
ac2_X <- as.matrix(X) %*% ac$xcoef[, 2]
ac2_Y <- as.matrix(Y) %*% ac$ycoef[, 2]

#2.- Agregamos las variables generadas a la matriz original de penguins
ac_df <- iris %>% 
  mutate(ac1_X=ac1_X,
         ac1_Y=ac1_Y,
         ac2_X=ac2_X,
         ac2_Y=ac2_Y)

#3.- Visualización de los nombres de las variables
colnames(ac_df)

#4.- Generación del grafico scater plot para la visualización de X1 y Y1
ac_df %>% 
  ggplot(aes(x=ac1_X,y=ac1_Y))+
  geom_point(color="indianred1") +
  xlab("Longitud del Sépalo (canónica)") +
  ylab("Longitud del Pétalo (canónica)") +
  ggtitle("Scater plot de X1 y Y1 canónicos")

#5.- Generación de un boxplot
ac_df %>% 
  ggplot(aes(x=Species,y=ac1_X, color=Species))+
  geom_boxplot(width=0.5)+
  geom_jitter(width=0.15)+
  ggtitle("Variable Canónica X1 contra Especie")

#Se observa una correlacion entre la variable canónica X1 y la variable latente Especie.

ac_df %>% 
  ggplot(aes(x=Species,y=ac1_Y, color=Species))+
  geom_boxplot(width=0.5)+
  geom_jitter(width=0.15)+
  ggtitle("Variable Canónica Y1 contra Especie")

#6.- Construcción de un scater plot con las variables X1 y Y1, separados por especie
ac_df %>% 
  ggplot(aes(x=ac1_X,y=ac1_Y, color=Species))+
  geom_point()+
  ggtitle("Variable Canónica X1 contra Y1")

#7.- Scater plot con las variables canónicas X2 y Y2 separadas por género.
ac_df %>% 
  ggplot(aes(x=ac2_X,y=ac2_Y, color=Species))+
  geom_point()+
  ggtitle("Variable Canónica X2 contra Y2")

#No se identifica correlación entre el conjunto de variables X2 y Y2 separadas por género.

#8.- Generación de la ecuación canónica
ac$xcoef
ac$ycoef

#9.- Sustitución en la ecuación canónica general
#**U1** = -0.072(Sepal.Length) + 0.030 (Sepal.Width)

#**V1** = -0.122(Petal.Length) + 0.043(Petal.Width)