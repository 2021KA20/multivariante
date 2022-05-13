### ANÁLISIS FACTORIAL - PSICOLOGÍA ###

# Descarga de paquetes y librerías
install.packages("psych")
library(psych)
install.packages("polycor")
library(polycor)
install.packages("ggcorrplot")
library(ggcorrplot)

# Matriz de trabajo
#Para esta práctica, se trabajó con la matriz **bfi** 
#del paquete *psych*, la cual se encuentra precargada en R. 

## 1.- Extracción de datos
x <- bfi

# Exploración de la matriz
## 1.- Dimensión
dim (x)

# Esta base de datos contiene 2800 observaciones y 28 variables.

## 2.- Tipos de variables
str(x)

## 3.- Nombre de las variables
colnames(x)

## 4.- Creación de una nueva matriz de datos en donde se 
#incluyan las variables 1 a la 25 y las primeras 200 observaciones
x1 <- bfi[1:200, 1:25]

# Matriz de correlaciones
R <- hetcor(x1)$correlations

## 1.- Gráfico de correlaciones
ggcorrplot(R, type = "lower", hc.order = TRUE)

#En el gráfico anterior, se muestran aquellos bloques 
#con correlaciones positivas y negativas. 
#Los espacios en blanco indican que las variables no se 
#correlacionan con alguna otra.

# Factorización de la matriz de correlaciones
#Se utiliza la prueba de esfericidad de Bartlett.
p_Bartlett <- cortest.bartlett(R)

## 1.- Visualización del p valor
p_Bartlett$p.value

#Con base en las siguientes hipótesis:
#H0: Las variables están correlacionadas.
#H1: Las variables no están correlacionadas.

#La decisión a utilizar es: No se rechaza H0, 
#debido a que el p-valor es pequeño, siendo que las 
#variables están correlacionadas.

# Criterio Kaiser-Meyer-Olkin
#Este criterio permite verificar si los datos que se 
#van a analizar son adecuados para un análisis factorial.

#0.00 a 0.49 -> No adecuados
#0.50 a 0.59 -> Poco adecuados
#0.60 a 0.69 -> Aceptables
#0.70 a 0.89 -> Buenos
#0.90 a 1.00 -> Excelente

KMO(R)

#De acuerdo con el indicador Overall MSA =  0.76, 
#los datos son buenos para aplicar análisis factorial. 

# Extracción de factores
#minres: mínimo residuo
#mle: max verosimilitud
#paf: ejes principales
#alpha: alfa
#minchi: mínimos cuadrados
#minrak: rango mínimo

modelo1 <- fa(R, nfactor = 3, rotate = "none", fm = "mle")

modelo2 <- fa(R, nfactor = 3, rotate = "none", fm = "minres")

## 1.- Extracción del resultado de las Comunidalidades
#Ahí se encuentra la proporción de la varianza explicada. 
#Se interpreta de tal forma qué número cercanos a 1, 
#el factor explica mejor la variable.
C1 <- sort(modelo1$communality, decreasing = TRUE)

C2 <- sort(modelo2$communality, decreasing = TRUE)

head(cbind(C1, C2))

## 2.- Extracción de Unicidades
#La unicidad es el cuadrado del coeficiente del factor único 
#y se expresa como la proporción de la varianza explicada 
#por el factor único. Es decir, no puede ser explicada 
#por otros factores. 

u1 <- sort(modelo1$uniquenesses, decreasing = TRUE)

u2 <- sort(modelo2$uniquenesses, decreasing = TRUE)

head(cbind(u1, u2))

scree(R)

# Rotación de la matriz
install.packages("GPArotation")
library(GPArotation)

rot <- c("None", "Varimax", "Quartimax", "Promax")
bi_mod <- function(tipo) {
  biplot.psych(fa(x1, nfactors = 2,
                  fm ="minres", rotate = tipo),
               main = paste("Biplot con rotación", tipo), 
               col = c(2,3,4), pch = c(21, 18, group = bfi[, "gender"]))
}
sapply(rot, bi_mod)

# Interpretación
#Para esto se utiliza el gráfico de árbol.

modelo_varimax <- fa(R, nfactor = 5,
                     rotate = "varimax", 
                     fm = "minres")

fa.diagram(modelo_varimax)

#Las líneas rojas representan a las cargas negativas y 
#las líneas negras a las cargas positivas.

## Visualización de la matriz de carga rotada
print(modelo_varimax$loadings, cut=0)
