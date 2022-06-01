# ANÁLISIS DE COMPONENTES PRINCIPALES Y ÁRBOLES DE DECISIÓN

## Introducción
#El **análisis de componentes principales**, también conocido 
#como *Principal Component Analysis* o PCA, es uno de los 
#algoritmos de selección de características más habituales. 
#Este análisis consiste en una técnica de selección de 
#características concreta que utiliza una transformación 
#ortogonal para convertir un conjunto de observaciones de 
#variables, posiblemente correlacionadas, en un conjunto más 
#reducido de variables que ya no guardan correlación y que se 
#conocen como *componentes principales*. 

#Su objetivo principal es representar adecuadamente la 
#información contenida en una matriz *n x p* con un número 
#menor de variables construidas a partir de combinaciones 
#lineales de las originales. Una de sus ventajas es que 
#permite una representación óptima, en un espacio de dimensión 
#reducida, de las observaciones originales.

#Por otra parte, un **árbol de decisión** es un esquema en el 
#que se encuentran todas las posibles consecuencias lógicas de
#realizar una secuencia de acciones. Se basa en los principios
#de clasificación que predicen el resultado de una decisión, 
#dando lugar a diferentes ramas de un árbol.
#Parte de una raíz, que gradualmente tiene diferentes nodos de 
#decisión. La estructura tiene nodos de terminación al final.




## Descripción de la matriz de datos
#La base de datos utilizada es obtenida del repositorio de 
#R en la paquetería *MASS* y contiene 200 filas y 8 columnas, 
#describiendo 5 medidas morfológicas en 50 cangrejos,
#cada uno de dos formas de color y ambos sexos, de la especie 
#Leptograpsus variegatus recolectada en Fremantle, W. Australia.

#Los datos contienen las siguientes columnas:
  
#1.- **sp:** species - "B" para azul "O" para naranja.

#2.- **sex:** F o M.

#3.- **index:** dentro de cada uno de los cuatro grupos. 1:50

#4.- **FL:** tamaño del lóbulo frontal (mm).

#5.- **RW:** anchura trasera (mm).

#6.- **CL:** longitud del caparazón (mm).

#7.- **CW:** ancho del caparazón (mm).

#8.- **BD:** profundidad corporal (mm).




## Exploración de la matriz de datos
#Paquetería a utilizar
install.packages("MASS")

#1.- Cargando la base de datos 
data(crabs, package = "MASS")
base <- crabs
head(base)

#2.- Dimensión de la base
dim(base)

#La base de datos contiene 200 observaciones y 8 variables.

#3.- Nombre de las variables
colnames(base)

#4.- Tipo de variables
str(base)

#5.- Presencia de NA
anyNA(base)

#Esta base no contiene datos nulos.




## Tratamiento de la matriz
#Paquetería y librería a utilizar 
install.packages("tidyverse")
library(tidyverse)

#1.- Cambio de etiquetas
base <- base %>% rename("Especies" = sp, "Sexo" = sex, "índice" = index,
                        "Tamaño del lóbulo frontal" = FL, "Anchura trasera" = RW, 
                        "Longitud del caparazón" = CL,
                        "Ancho del caparazón" = CW, "Profundidad corporal" = BD)
base <- base %>% mutate(Especies =  recode(Especies, 'B' = "Blue",
                                           'O' = "Orange"))
base <- base %>% mutate(Sexo =  recode(Sexo, 'M' = "Macho",
                                       'F' = "Hembra"))
head(base)

#2.- Resumen de los datos
summary(base)

#3.- Configuración y/o filtrado de variables
#Se genera una nueva matriz *x1* que filtrará las variables 
#cuantitativas de especie naranja, eliminando las variables 
#Especies, Sexo e índice
x1 <- base[101:200,-cbind(1,2,3)] 
head(x1)




# METODOLOGÍA DE ANÁLISIS
#El primer componente principal será la *combinación lineal*
#de las variables originales que tengan máxima varianza y los
#valores de los *n* individuos se representan mediante el 
#vector **z1**. Al tratarse de las variables originales,
#su media es cero. 

#Se calcula la matriz de covarianza muestral y la matriz 
#de correlaciones de las variables originales, donde se 
#obtienen los valores y vectores propios. Mediante ellos, 
#se calcula la proporción de variabilidad y la proporción 
#de variabilidad acumulada y es en este último donde 
#se obtienen aquellos factores que conforman el número de 
#componentes, considerando el 80% de la varianza explicada.
#La obtención de los coeficientes se realiza mediante la matriz 
#de autovectores.

#Para la elaboración de un árbol de decisión, se deja la
#columna categórica y las variables cuantitativas que sean
#de interés y se consiguen aquellas variables para elaborar 
#el árbol. Como el árbol es de todos los datos, se escoge un 
#tamaño de muestra que será de entrenamiento y se calcula la 
#probabilidad de predicción y la matriz de confusión para 
#saber con cuántos casos se equivoca. 




# RESULTADOS
## PCA paso a paso

#1.- Se transforma la matriz en un data.frame 
x1 <- as.data.frame(x1)

#2.- Definir n(individuos) y p(variables)
n <- dim(x1)[1]
p <- dim(x1)[2]

#3.- Generación de un scatterplot de las variables originales, sin tomar en cuenta la variable cualitativa (Sexo)
pairs(x1,col="blue", pch=19, 
      main="Variables originales")

#4.- Obtención de la media por columna y la **matriz de covarianza muestral**
mu <-colMeans(x1)
mu

s <-cov(x1)
s

#5.- Obtención de los **valores** y **vectores propios** desde la matriz de covarianza muestral
es <- eigen(s)
es

#5.1.- Separación de la matriz de valores propios:
eigen.val <-es$values
eigen.val

#5.2.- Separación de la matriz de vectores propios:
eigen.vec <-es$vectors
eigen.vec

#6.- Proporción de variabilidad para cada valor

#6.1.- Para la matriz de valores propios:
pro.var<-eigen.val/sum(eigen.val)
pro.var

#6.2.- Proporción de variabilidad acumulada:
pro.var.acum<-cumsum(eigen.val)/sum(eigen.val)
pro.var.acum

#7.- Obtención de la matriz de correlaciones
R<-cor(x1)
R

#8.- Obtención de los *valores* y *vectores propios* a partir de la **matriz de correlaciones**
eR<-eigen(R)
eR

#9.- Separación de la matriz de valores y vectores propios

#9.1.- Separación de la matriz de valores propios:
eigen.val.R<-eR$values
eigen.val.R

#9.2.- Separación de la matriz de vectores propios:
eigen.vec.R<-eR$vectors
eigen.vec.R

#10.- Cálculo de la proporción de variabilidad 

#10.1.- Para la matriz de valores propios:
pro.var.R<-eigen.val/sum(eigen.val)
pro.var.R

#10.2.- Proporción de variabilidad acumulada:
  
#En este punto, se selecciona el número de componentes 
#siguiendo el criterio del 80% de la varianza explicada.
pro.var.acum.R<-cumsum(eigen.val)/sum(eigen.val)
pro.var.acum.R

#En este caso, se selecciona 1 factor (0.985% de varianza explicada)

#11.- Cálculo de la media de los valores propios
mean(eigen.val.R)

#12.- Obtención de coeficientes

#Centrar los datos con respecto a la media.

#12.1.- Construcción de matriz de 1:
ones<-matrix(rep(1,n),nrow=n, ncol=1)

#12.2.- Construcción de la matriz centrada:
X.cen<-as.matrix(x1)-ones%*%mu

#13.- Construcción de la matriz diagonal de las covarianzas
Dx<-diag(diag(s))
Dx

#14.- Construcción de la matriz centrada multiplicada por Dx^1/2
Y<-X.cen%*%solve(Dx)^(1/2)

#15.- Construcción de los coeficientes o scores eigen.vec.R matriz de autovectores
scores<-Y%*%eigen.vec.R
scores

#16.- Se nombran las columnas PC1...PC5
colnames(scores)<-c("PC1","PC2","PC3","PC4", "PC5")

#17.- Visualización de los scores
scores[1:10,]

#18.- Generación del gráfico de los scores
pairs(scores, main="scores", col="brown", pch=10)

## Análisis de componentes principales vía sintetizada
#1.- Cálculo de la varianza a las columnas:  1 = filas, 2 = columnas
apply(x1, 2, var)

#2.- Aplicación de la función **prcomp** para reducir la dimensionalidad y centrado por la media y escalada por la desviacion standar (dividir entre sd).
acp<-prcomp(x1, center=TRUE, scale=TRUE)
acp

#3.- Generación del gráfico **screeplot**
plot(acp, type="l", main = "Componentes principales")

#4.- Visualización del resumen de la matriz **ACP**
summary(acp)




## Árboles de decisión
#Paquetería y librería a utilizar 
install.packages("DMwR2")
library(DMwR2)

#1.- Se utiliza una semilla
set.seed(1234)
data(crabs, package="MASS")

#2.- Se deja la columna *sp* y se elimina la columna *sex* e *index* puesto que no nos interesan
crabs <- crabs[,-cbind(2,3)] 

#3.- Se consiguen aquellas variables para elaborar el árbol
ct1<-rpartXse(sp ~., crabs)
ct1

#El tamaño de *n* son 200 cangrejos y aquellas variables
#marcadas en asterisco son aquellas variables significativas
#que servirán para la elaboración del árbol.

#Elaboración del árbol:
#Paquetería y librería a utilizar 
install.packages("rpart.plot")
library(rpart.plot)
library(rpart)

#1.- Árbol de decisión
prp(ct1, type=0, extra=101)

#Para comprender el árbol, algunos puntos a considerar son:
  
#-Los cuadritos son los nodos.

#-Las líneas son los arcos.

#-Las palabras en negritas son las condiciones.

#-Si se cambia la semilla, el árbol cambia.

#-Si no se pone una semilla, igual el árbol cambia.

#-Lo que se hace es tomar una muestra de datos para esa semilla.


#El árbol presenta aquellas decisiones que se 
#tomarán para clasificar a los cangrejos, por ejemplo: 
#Si el tamaño del lóbulo frontal *(FL)* es menor que 17 entonces,
#si el ancho del caparazón *(CW)* es mayor o igual que 36 mm, 
#se clasifica a 29 cangrejos en la especie azul, de lo contrario,
#si la longitud del caparazón *(CL)* es mayor o igual que 36 mm,
#se clasifica a 7 cangrejos en la especie azul y si no, 
#4 cangrejos en la especie naranja. 
#En dado caso de que el tamaño del lóbulo frontal *(FL)* 
#no sea menor que 17 mm y si el ancho del caparazón *(CW)* 
#es mayor o igual a 44 mm, entonces, si el tamaño del lóbulo frontal
#*(FL)* es menor que 20 mm, se clasificará a 11 cangrejos en 
##la especie azul y si no, se clasificarán 21 cangrejos en la
## especie naranja y 1 en la especie azul. 


set.seed(1234)

#2.- Se utiliza un tamaño de muestra 100
rndSample<-sample(1:nrow(crabs), 100) 

#3.- Muestra de datos/entrenamiento
tr <- crabs[rndSample,] 

#4.- Muestra de prueba
ts <- crabs[-rndSample, ] 
ct <- rpartXse(sp ~., tr, se=0.5)

#5.- Probabilidad de predicción con respecto a la muestra de entrenamiento
ps1<- predict(ct, ts) 
head(ps1)

#6.- Árbol de decisión
prp(ct, type=0, extra=101) 

#El árbol presenta un tamaño de muestra de 
#100 cangrejos, las decisiones a tomar para clasificarlos son: 
#Si la profundidad corporal *(BD)* es menor que 17 mm y si 
#es menor que 12 mm, se clasifica a 23 cangrejos en la especie
#azul y 5 en la especie naranja. Si la profundidad corporal *(BD)*
#no es menor que 17 mm, se clasifica a 21 cangrejos en la especie 
#naranja y 2 en la especie azul.


ps2<-predict(ct, ts, type="class")
head(ps2)

#7.- Matriz de confusión para ver con cuántos casos se equivoca
(cm<-table(ps2, ts$sp)) 

#8.- Número de error
100*(1-sum(diag(cm))/sum(cm))

#Se equivoca con 18 cangrejos.

#9.- Evaluando con los datos que generaron el modelo
ps3<-predict(ct, tr, type="class")
head(ps3)

#10.- Matriz de confusión para ver con cuántos casos se equivoca
(cm<-table(ps3, tr$sp)) 

#11.- Error
100*(1-sum(diag(cm))/sum(cm)) 




# CONCLUSIÓN
#1.- Construcción del modelo de componentes principales:
  
  #**z1** = 0.454*(Tamaño del lóbulo frontal)* + 0.422*(Anchura trasera)* +    0.453*(Longitud del caparazón)* + 0.455*(Ancho del caparazón)* + 0.450*(Profundidad corporal)*
  
#2.- Interpretación del resultado:
  
#Este componente conforma a la especie naranja de cangrejos.
#El modelo con un sólo componente principal, distingue entre 
#el tamaño del lóbulo frontal, la anchura trasera, la longitud 
#y el ancho del caparazón y la profundidad corporal que tienen
#los cangrejos de la especie naranja que de la especie azul. 
#Como se explica más del 80% de la varianza, basta con que sea
#un sólo componente principal.


#Los árboles de decisión mostraron aquellas decisiones a 
#considerar para clasificar a cada cangrejo.
#Al considerar una muestra de 100 cangrejos, el árbol arrojó 
#pocas decisiones, debido a que se trataba de la mitad de los
#datos. De igual manera, se puede ver el porcentaje que 
#pertenece cada cangrejo en dicha clasificación. 




# REFERENCIAS
#(s.a). (s.f) *Análisis de componentes principales (ACP): Principal component analysis (PCA). ¿Qué es el análisis de componentes principales?*. Compañia Telefónica Tech. Recuperado de: https://aiofthings.telefonicatech.com/recursos/datapedia/analisis-componentes-principales


#López, J. F., (2019). *Árbol de decisión*. Economipedia.com. Recuperado de: https://economipedia.com/definiciones/arbol-de-decision.html


#(s.a). (s.f). *Qué es un árbol de decisión y ejemplos*. edraw: A Wondershare Company. Recuperado de: https://www.edrawsoft.com/es/decision-tree/
  
  
#Milborrowm, S. (2021). *rpart.plot: Plot 'rpart' Models: An Enhanced Version of 'plot.rpart'.* R package version 3.1.0. https://CRAN.R-project.org/package=rpart.plot


#Torgo, L. (2016). *Data Mining with R, learning with case studies.* 2nd editionChapman and Hall/CRC. URL:http://ltorgo.github.io/DMwR2


#Venables, W. N. & Ripley, B. D. (2002). *Modern Applied Statistics with S.* Fourth Edition. Springer, New York. ISBN 0-387-95457-0


#Wickham et al., (2019). *Welcome to the tidyverse.* Journal of Open Source Software, 4(43), 1686, https://doi.org/10.21105/joss.01686


