#_____ Dendrograma____#

## Paqueter�a y librer�a a utilizar
install.packages("cluster.datasets")
library(cluster.datasets)

# Matriz de datos
#La base de datos a utilizar es *all.us.city.crime.1970* y 
#est� precargada en R en la paqueter�a **cluster.datasets**, 
#contiene los registros del crimen de la ciudad junto con las 
#estad�sticas de poblaci�n, con 24 observaciones y 10 variables.

data("all.us.city.crime.1970")

# Tratamiento y exploraci�n de la matriz
AMM=all.us.city.crime.1970
head(AMM)

## 1.- Dimensi�n de la base
dim(AMM)

## 2.- Tipo de variables
str(AMM)

## 3.- Saber si existen datos nulos
anyNA(AMM)

#Esta matriz no contiene datos nulos.

## 4.- Resumen de los datos
summary(AMM)

## 5.- C�lculo de la matriz de distancia de Mahalonobis
dist.AMM<-dist(AMM[,2:10])

## 6.- Convertir los resultados del  c�lculo de la distancia a una matriz de datos y me indique 3 d�gitos.
round(as.matrix(dist.AMM)[1:10, 1:10],3)

## 7.- C�lculo del dendrograma
dend.AMM<-as.dendrogram(hclust(dist.AMM))

## 8.- Generaci�n del dendrograma
plot(dend.AMM)

## 9.- Agregar etiquetas al gr�fico
AMM.nombres=AMM
rownames(AMM.nombres)= AMM.nombres$city
AMM.nombres=AMM.nombres[,-1]

## 10.- Construcci�n del nuevo gr�fico
plot(as.dendrogram(hclust(dist(AMM.nombres))))

#  Modificando el dendrograma
## Paqueter�a y librer�a a utilizar
install.packages("dendextend")
library(dendextend)

## 1.- Guardar las etiquetas en un objeto "L"
L=labels(dend.AMM)
labels(dend.AMM)=AMM$city[L]

## 2.- Cambiar el tama�o de las etiquetas
dend.AMM %>%
  set(what="labels_col", "purple") %>% 
  set(what="labels_cex", 0.8) %>% 
  plot(main="Dendrograma de ciudades") 

#El dendrograma anterior muestra aquellas ciudades que 
#pertenecen o no a un mismo grupo de clasificaci�n.
#Las ciudades de Minneapolis, St. Louis, Baltimore, Houston y 
#Newark conforman un s�lo grupo, mientras que, 
#las ciudades de Cleveland, Boston y Pitsburgh conforman 
#otro grupo. Estas ciudades y todas las dem�s que est�n 
#clasificadas por grupos, tienen una o m�s caracter�sticas 
#similares que hacen que sean de la misma clasificaci�n. 
