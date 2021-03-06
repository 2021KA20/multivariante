# AN�LISIS DE COMPONENTES PRINCIPALES Y �RBOLES DE DECISI�N

## Introducci�n
#El **an�lisis de componentes principales**, tambi�n conocido 
#como *Principal Component Analysis* o PCA, es uno de los 
#algoritmos de selecci�n de caracter�sticas m�s habituales. 
#Este an�lisis consiste en una t�cnica de selecci�n de 
#caracter�sticas concreta que utiliza una transformaci�n 
#ortogonal para convertir un conjunto de observaciones de 
#variables, posiblemente correlacionadas, en un conjunto m�s 
#reducido de variables que ya no guardan correlaci�n y que se 
#conocen como *componentes principales*. 

#Su objetivo principal es representar adecuadamente la 
#informaci�n contenida en una matriz *n x p* con un n�mero 
#menor de variables construidas a partir de combinaciones 
#lineales de las originales. Una de sus ventajas es que 
#permite una representaci�n �ptima, en un espacio de dimensi�n 
#reducida, de las observaciones originales.

#Por otra parte, un **�rbol de decisi�n** es un esquema en el 
#que se encuentran todas las posibles consecuencias l�gicas de
#realizar una secuencia de acciones. Se basa en los principios
#de clasificaci�n que predicen el resultado de una decisi�n, 
#dando lugar a diferentes ramas de un �rbol.
#Parte de una ra�z, que gradualmente tiene diferentes nodos de 
#decisi�n. La estructura tiene nodos de terminaci�n al final.




## Descripci�n de la matriz de datos
#La base de datos utilizada es obtenida del repositorio de 
#R en la paqueter�a *MASS* y contiene 200 filas y 8 columnas, 
#describiendo 5 medidas morfol�gicas en 50 cangrejos,
#cada uno de dos formas de color y ambos sexos, de la especie 
#Leptograpsus variegatus recolectada en Fremantle, W. Australia.

#Los datos contienen las siguientes columnas:
  
#1.- **sp:** species - "B" para azul "O" para naranja.

#2.- **sex:** F o M.

#3.- **index:** dentro de cada uno de los cuatro grupos. 1:50

#4.- **FL:** tama�o del l�bulo frontal (mm).

#5.- **RW:** anchura trasera (mm).

#6.- **CL:** longitud del caparaz�n (mm).

#7.- **CW:** ancho del caparaz�n (mm).

#8.- **BD:** profundidad corporal (mm).




## Exploraci�n de la matriz de datos
#Paqueter�a a utilizar
install.packages("MASS")

#1.- Cargando la base de datos 
data(crabs, package = "MASS")
base <- crabs
head(base)

#2.- Dimensi�n de la base
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
#Paqueter�a y librer�a a utilizar 
install.packages("tidyverse")
library(tidyverse)

#1.- Cambio de etiquetas
base <- base %>% rename("Especies" = sp, "Sexo" = sex, "�ndice" = index,
                        "Tama�o del l�bulo frontal" = FL, "Anchura trasera" = RW, 
                        "Longitud del caparaz�n" = CL,
                        "Ancho del caparaz�n" = CW, "Profundidad corporal" = BD)
base <- base %>% mutate(Especies =  recode(Especies, 'B' = "Blue",
                                           'O' = "Orange"))
base <- base %>% mutate(Sexo =  recode(Sexo, 'M' = "Macho",
                                       'F' = "Hembra"))
head(base)

#2.- Resumen de los datos
summary(base)

#3.- Configuraci�n y/o filtrado de variables
#Se genera una nueva matriz *x1* que filtrar� las variables 
#cuantitativas de especie naranja, eliminando las variables 
#Especies, Sexo e �ndice
x1 <- base[101:200,-cbind(1,2,3)] 
head(x1)




# METODOLOG�A DE AN�LISIS
#El primer componente principal ser� la *combinaci�n lineal*
#de las variables originales que tengan m�xima varianza y los
#valores de los *n* individuos se representan mediante el 
#vector **z1**. Al tratarse de las variables originales,
#su media es cero. 

#Se calcula la matriz de covarianza muestral y la matriz 
#de correlaciones de las variables originales, donde se 
#obtienen los valores y vectores propios. Mediante ellos, 
#se calcula la proporci�n de variabilidad y la proporci�n 
#de variabilidad acumulada y es en este �ltimo donde 
#se obtienen aquellos factores que conforman el n�mero de 
#componentes, considerando el 80% de la varianza explicada.
#La obtenci�n de los coeficientes se realiza mediante la matriz 
#de autovectores.

#Para la elaboraci�n de un �rbol de decisi�n, se deja la
#columna categ�rica y las variables cuantitativas que sean
#de inter�s y se consiguen aquellas variables para elaborar 
#el �rbol. Como el �rbol es de todos los datos, se escoge un 
#tama�o de muestra que ser� de entrenamiento y se calcula la 
#probabilidad de predicci�n y la matriz de confusi�n para 
#saber con cu�ntos casos se equivoca. 




# RESULTADOS
## PCA paso a paso

#1.- Se transforma la matriz en un data.frame 
x1 <- as.data.frame(x1)

#2.- Definir n(individuos) y p(variables)
n <- dim(x1)[1]
p <- dim(x1)[2]

#3.- Generaci�n de un scatterplot de las variables originales, sin tomar en cuenta la variable cualitativa (Sexo)
pairs(x1,col="blue", pch=19, 
      main="Variables originales")

#4.- Obtenci�n de la media por columna y la **matriz de covarianza muestral**
mu <-colMeans(x1)
mu

s <-cov(x1)
s

#5.- Obtenci�n de los **valores** y **vectores propios** desde la matriz de covarianza muestral
es <- eigen(s)
es

#5.1.- Separaci�n de la matriz de valores propios:
eigen.val <-es$values
eigen.val

#5.2.- Separaci�n de la matriz de vectores propios:
eigen.vec <-es$vectors
eigen.vec

#6.- Proporci�n de variabilidad para cada valor

#6.1.- Para la matriz de valores propios:
pro.var<-eigen.val/sum(eigen.val)
pro.var

#6.2.- Proporci�n de variabilidad acumulada:
pro.var.acum<-cumsum(eigen.val)/sum(eigen.val)
pro.var.acum

#7.- Obtenci�n de la matriz de correlaciones
R<-cor(x1)
R

#8.- Obtenci�n de los *valores* y *vectores propios* a partir de la **matriz de correlaciones**
eR<-eigen(R)
eR

#9.- Separaci�n de la matriz de valores y vectores propios

#9.1.- Separaci�n de la matriz de valores propios:
eigen.val.R<-eR$values
eigen.val.R

#9.2.- Separaci�n de la matriz de vectores propios:
eigen.vec.R<-eR$vectors
eigen.vec.R

#10.- C�lculo de la proporci�n de variabilidad 

#10.1.- Para la matriz de valores propios:
pro.var.R<-eigen.val/sum(eigen.val)
pro.var.R

#10.2.- Proporci�n de variabilidad acumulada:
  
#En este punto, se selecciona el n�mero de componentes 
#siguiendo el criterio del 80% de la varianza explicada.
pro.var.acum.R<-cumsum(eigen.val)/sum(eigen.val)
pro.var.acum.R

#En este caso, se selecciona 1 factor (0.985% de varianza explicada)

#11.- C�lculo de la media de los valores propios
mean(eigen.val.R)

#12.- Obtenci�n de coeficientes

#Centrar los datos con respecto a la media.

#12.1.- Construcci�n de matriz de 1:
ones<-matrix(rep(1,n),nrow=n, ncol=1)

#12.2.- Construcci�n de la matriz centrada:
X.cen<-as.matrix(x1)-ones%*%mu

#13.- Construcci�n de la matriz diagonal de las covarianzas
Dx<-diag(diag(s))
Dx

#14.- Construcci�n de la matriz centrada multiplicada por Dx^1/2
Y<-X.cen%*%solve(Dx)^(1/2)

#15.- Construcci�n de los coeficientes o scores eigen.vec.R matriz de autovectores
scores<-Y%*%eigen.vec.R
scores

#16.- Se nombran las columnas PC1...PC5
colnames(scores)<-c("PC1","PC2","PC3","PC4", "PC5")

#17.- Visualizaci�n de los scores
scores[1:10,]

#18.- Generaci�n del gr�fico de los scores
pairs(scores, main="scores", col="brown", pch=10)

## An�lisis de componentes principales v�a sintetizada
#1.- C�lculo de la varianza a las columnas:  1 = filas, 2 = columnas
apply(x1, 2, var)

#2.- Aplicaci�n de la funci�n **prcomp** para reducir la dimensionalidad y centrado por la media y escalada por la desviacion standar (dividir entre sd).
acp<-prcomp(x1, center=TRUE, scale=TRUE)
acp

#3.- Generaci�n del gr�fico **screeplot**
plot(acp, type="l", main = "Componentes principales")

#4.- Visualizaci�n del resumen de la matriz **ACP**
summary(acp)




## �rboles de decisi�n
#Paqueter�a y librer�a a utilizar 
install.packages("DMwR2")
library(DMwR2)

#1.- Se utiliza una semilla
set.seed(1234)
data(crabs, package="MASS")

#2.- Se deja la columna *sp* y se elimina la columna *sex* e *index* puesto que no nos interesan
crabs <- crabs[,-cbind(2,3)] 

#3.- Se consiguen aquellas variables para elaborar el �rbol
ct1<-rpartXse(sp ~., crabs)
ct1

#El tama�o de *n* son 200 cangrejos y aquellas variables
#marcadas en asterisco son aquellas variables significativas
#que servir�n para la elaboraci�n del �rbol.

#Elaboraci�n del �rbol:
#Paqueter�a y librer�a a utilizar 
install.packages("rpart.plot")
library(rpart.plot)
library(rpart)

#1.- �rbol de decisi�n
prp(ct1, type=0, extra=101)

#Para comprender el �rbol, algunos puntos a considerar son:
  
#-Los cuadritos son los nodos.

#-Las l�neas son los arcos.

#-Las palabras en negritas son las condiciones.

#-Si se cambia la semilla, el �rbol cambia.

#-Si no se pone una semilla, igual el �rbol cambia.

#-Lo que se hace es tomar una muestra de datos para esa semilla.


#El �rbol presenta aquellas decisiones que se 
#tomar�n para clasificar a los cangrejos, por ejemplo: 
#Si el tama�o del l�bulo frontal *(FL)* es menor que 17 entonces,
#si el ancho del caparaz�n *(CW)* es mayor o igual que 36 mm, 
#se clasifica a 29 cangrejos en la especie azul, de lo contrario,
#si la longitud del caparaz�n *(CL)* es mayor o igual que 36 mm,
#se clasifica a 7 cangrejos en la especie azul y si no, 
#4 cangrejos en la especie naranja. 
#En dado caso de que el tama�o del l�bulo frontal *(FL)* 
#no sea menor que 17 mm y si el ancho del caparaz�n *(CW)* 
#es mayor o igual a 44 mm, entonces, si el tama�o del l�bulo frontal
#*(FL)* es menor que 20 mm, se clasificar� a 11 cangrejos en 
##la especie azul y si no, se clasificar�n 21 cangrejos en la
## especie naranja y 1 en la especie azul. 


set.seed(1234)

#2.- Se utiliza un tama�o de muestra 100
rndSample<-sample(1:nrow(crabs), 100) 

#3.- Muestra de datos/entrenamiento
tr <- crabs[rndSample,] 

#4.- Muestra de prueba
ts <- crabs[-rndSample, ] 
ct <- rpartXse(sp ~., tr, se=0.5)

#5.- Probabilidad de predicci�n con respecto a la muestra de entrenamiento
ps1<- predict(ct, ts) 
head(ps1)

#6.- �rbol de decisi�n
prp(ct, type=0, extra=101) 

#El �rbol presenta un tama�o de muestra de 
#100 cangrejos, las decisiones a tomar para clasificarlos son: 
#Si la profundidad corporal *(BD)* es menor que 17 mm y si 
#es menor que 12 mm, se clasifica a 23 cangrejos en la especie
#azul y 5 en la especie naranja. Si la profundidad corporal *(BD)*
#no es menor que 17 mm, se clasifica a 21 cangrejos en la especie 
#naranja y 2 en la especie azul.


ps2<-predict(ct, ts, type="class")
head(ps2)

#7.- Matriz de confusi�n para ver con cu�ntos casos se equivoca
(cm<-table(ps2, ts$sp)) 

#8.- N�mero de error
100*(1-sum(diag(cm))/sum(cm))

#Se equivoca con 18 cangrejos.

#9.- Evaluando con los datos que generaron el modelo
ps3<-predict(ct, tr, type="class")
head(ps3)

#10.- Matriz de confusi�n para ver con cu�ntos casos se equivoca
(cm<-table(ps3, tr$sp)) 

#11.- Error
100*(1-sum(diag(cm))/sum(cm)) 




# CONCLUSI�N
#1.- Construcci�n del modelo de componentes principales:
  
  #**z1** = 0.454*(Tama�o del l�bulo frontal)* + 0.422*(Anchura trasera)* +    0.453*(Longitud del caparaz�n)* + 0.455*(Ancho del caparaz�n)* + 0.450*(Profundidad corporal)*
  
#2.- Interpretaci�n del resultado:
  
#Este componente conforma a la especie naranja de cangrejos.
#El modelo con un s�lo componente principal, distingue entre 
#el tama�o del l�bulo frontal, la anchura trasera, la longitud 
#y el ancho del caparaz�n y la profundidad corporal que tienen
#los cangrejos de la especie naranja que de la especie azul. 
#Como se explica m�s del 80% de la varianza, basta con que sea
#un s�lo componente principal.


#Los �rboles de decisi�n mostraron aquellas decisiones a 
#considerar para clasificar a cada cangrejo.
#Al considerar una muestra de 100 cangrejos, el �rbol arroj� 
#pocas decisiones, debido a que se trataba de la mitad de los
#datos. De igual manera, se puede ver el porcentaje que 
#pertenece cada cangrejo en dicha clasificaci�n. 




# REFERENCIAS
#(s.a). (s.f) *An�lisis de componentes principales (ACP): Principal component analysis (PCA). �Qu� es el an�lisis de componentes principales?*. Compa�ia Telef�nica Tech. Recuperado de: https://aiofthings.telefonicatech.com/recursos/datapedia/analisis-componentes-principales


#L�pez, J. F., (2019). *�rbol de decisi�n*. Economipedia.com. Recuperado de: https://economipedia.com/definiciones/arbol-de-decision.html


#(s.a). (s.f). *Qu� es un �rbol de decisi�n y ejemplos*. edraw: A Wondershare Company. Recuperado de: https://www.edrawsoft.com/es/decision-tree/
  
  
#Milborrowm, S. (2021). *rpart.plot: Plot 'rpart' Models: An Enhanced Version of 'plot.rpart'.* R package version 3.1.0. https://CRAN.R-project.org/package=rpart.plot


#Torgo, L. (2016). *Data Mining with R, learning with case studies.* 2nd editionChapman and Hall/CRC. URL:http://ltorgo.github.io/DMwR2


#Venables, W. N. & Ripley, B. D. (2002). *Modern Applied Statistics with S.* Fourth Edition. Springer, New York. ISBN 0-387-95457-0


#Wickham et al., (2019). *Welcome to the tidyverse.* Journal of Open Source Software, 4(43), 1686, https://doi.org/10.21105/joss.01686


