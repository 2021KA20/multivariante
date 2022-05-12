#_____ Dendrograma____#

## Paquetería y librería a utilizar
install.packages("cluster.datasets")
library(cluster.datasets)

# Matriz de datos
#La base de datos a utilizar es *all.us.city.crime.1970* y 
#está precargada en R en la paquetería **cluster.datasets**, 
#contiene los registros del crimen de la ciudad junto con las 
#estadísticas de población, con 24 observaciones y 10 variables.

data("all.us.city.crime.1970")

# Tratamiento y exploración de la matriz
AMM=all.us.city.crime.1970
head(AMM)

## 1.- Dimensión de la base
dim(AMM)

## 2.- Tipo de variables
str(AMM)

## 3.- Saber si existen datos nulos
anyNA(AMM)

#Esta matriz no contiene datos nulos.

## 4.- Resumen de los datos
summary(AMM)

## 5.- Cálculo de la matriz de distancia de Mahalonobis
dist.AMM<-dist(AMM[,2:10])

## 6.- Convertir los resultados del  cálculo de la distancia a una matriz de datos y me indique 3 dígitos.
round(as.matrix(dist.AMM)[1:10, 1:10],3)

## 7.- Cálculo del dendrograma
dend.AMM<-as.dendrogram(hclust(dist.AMM))

## 8.- Generación del dendrograma
plot(dend.AMM)

## 9.- Agregar etiquetas al gráfico
AMM.nombres=AMM
rownames(AMM.nombres)= AMM.nombres$city
AMM.nombres=AMM.nombres[,-1]

## 10.- Construcción del nuevo gráfico
plot(as.dendrogram(hclust(dist(AMM.nombres))))

#  Modificando el dendrograma
## Paquetería y librería a utilizar
install.packages("dendextend")
library(dendextend)

## 1.- Guardar las etiquetas en un objeto "L"
L=labels(dend.AMM)
labels(dend.AMM)=AMM$city[L]

## 2.- Cambiar el tamaño de las etiquetas
dend.AMM %>%
  set(what="labels_col", "purple") %>% 
  set(what="labels_cex", 0.8) %>% 
  plot(main="Dendrograma de ciudades") 

#El dendrograma anterior muestra aquellas ciudades que 
#pertenecen o no a un mismo grupo de clasificación.
#Las ciudades de Minneapolis, St. Louis, Baltimore, Houston y 
#Newark conforman un sólo grupo, mientras que, 
#las ciudades de Cleveland, Boston y Pitsburgh conforman 
#otro grupo. Estas ciudades y todas las demás que están 
#clasificadas por grupos, tienen una o más características 
#similares que hacen que sean de la misma clasificación. 
