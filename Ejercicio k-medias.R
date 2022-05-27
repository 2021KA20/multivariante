
#__________ K-MEANS____________

# Cargar la matriz de datos.

X<-as.data.frame(state.x77)
colnames(X)

 
#-------------------------------------
#     Transformacion de datos
#-------------------------------------

#1.- Transformacion de las variables x1,x3 y x8
# con la funcion de logaritmo. Esto porque tienen números mayores.

X[,1]<-log(X[,1])
colnames(X)[1]<-"Log-Population"

X[,3]<-log(X[,3])
colnames(X)[3]<-"Log-Illiteracy"

X[,8]<-log(X[,8])
colnames(X)[8]<-"Log-Area"

#---------------------------------
#    Metodo k-means
#---------------------------------

#1.- Separacion de filas y columnas.

dim(X)
n<-dim(X)[1]
p<-dim(X)[2]

# 2.- Estandarizacion univariante.
X.s<-scale(X)

# 3.- Algoritmo k-medias (3 grupos)
# nstart: cantidad de subconjuntos aleatorios que se escogen
# para realizar los calculos de algoritmo.
Kmeans.3<-kmeans(X.s, 3, nstart=25)

# centroides
Kmeans.3$centers

# cluster de pertenencia
Kmeans.3$cluster


# 4.- SCDG
SCDG<-sum(Kmeans.3$withinss)
SCDG

# 5.- Clusters
cl.kmeans<-Kmeans.3$cluster
cl.kmeans

# 6.- Scatter plot con la division de grupos
# obtenidos (se utiliza la matriz de datos centrados).
col.cluster<-c("blue", "red", "green")[cl.kmeans]
pairs(X.s, col=col.cluster, main="k-means", pch=19)

#-----------------------------------------
#  Visualizacion con las dos componentes principales
#-----------------------------------------

library(cluster) 

clusplot(X.s, cl.kmeans, 
         main="Dos primeras componentes principales")

text(princomp(X.s)$score[,1:2],
     labels=rownames(X.s), pos=1, col="blue")

#-------------------------------------
#  Silhouette
#--------------------------------------
# Representacion grafica de la eficacia de
# clasificacion de una observacion dentro de un
# grupo.
# Mayor a 0.7 = mejor clasificación
# Entre más cercana a 1 es mejor.

# 1.- Generacion de los calculos
dist.Euc<-dist(X.s, method = "euclidean")
Sil.kmeans<-silhouette(cl.kmeans, dist.Euc)

#2.- Generacion del grafico
plot(Sil.kmeans, main="Silhouette for k-means", 
col="blue")



#--------------------
# EJERCICIO 
#--------------------
# 1.- Vas a replicar el script pero vas a sugerir
# un nuevo número de clústers diferente a 3 y 1.
# 2.- Incluir la interpretación del Silhouette.


##### 2 Clústers #####
# 3.- Algoritmo k-medias (2 grupos)
# nstart: cantidad de subconjuntos aleatorios que se escogen
# para realizar los calculos de algoritmo.
Kmeans.2<-kmeans(X.s, 2, nstart=25)

# centroides
Kmeans.2$centers

# cluster de pertenencia
Kmeans.2$cluster

# 4.- SCDG
SCDG<-sum(Kmeans.2$withinss)
SCDG

# 5.- Clusters
cl.kmeans<-Kmeans.2$cluster
cl.kmeans

# 6.- Scatter plot con la division de grupos
# obtenidos (se utiliza la matriz de datos centrados).
col.cluster<-c("blue", "red")[cl.kmeans]
pairs(X.s, col=col.cluster, main="k-means", pch=19)

#-----------------------------------------
#  Visualizacion con las dos componentes principales
#-----------------------------------------
library(cluster) 

clusplot(X.s, cl.kmeans, 
         main="Dos primeras componentes principales")

text(princomp(X.s)$score[,1:2],
     labels=rownames(X.s), pos=1, col="blue")

#-------------------------------------
#  Silhouette
#--------------------------------------
# Representacion grafica de la eficacia de
# clasificacion de una observacion dentro de un
# grupo.
# Mayor a 0.7 = mejor clasificación
# Entre más cercana a 1 es mejor.

# 1.- Generacion de los calculos
dist.Euc<-dist(X.s, method = "euclidean")
Sil.kmeans<-silhouette(cl.kmeans, dist.Euc)

#2.- Generacion del grafico
plot(Sil.kmeans, main="Silhouette for k-means", 
     col="blue")

### Con 2 clusters, hay 3 datos no clasificados.
### El ancho del Silhouette es de 0.3.
### Hay un cluster con una probabilidad del Silhouette buena y 
### otra con probabilidad un poco baja.

### Se probará ahora con 4.


##### 4 Clústers #####
# 3.- Algoritmo k-medias (4 grupos)
# nstart: cantidad de subconjuntos aleatorios que se escogen
# para realizar los calculos de algoritmo.
Kmeans.4<-kmeans(X.s, 4, nstart=25)

# centroides
Kmeans.4$centers

# cluster de pertenencia
Kmeans.4$cluster

# 4.- SCDG
SCDG<-sum(Kmeans.4$withinss)
SCDG

# 5.- Clusters
cl.kmeans<-Kmeans.4$cluster
cl.kmeans

# 6.- Scatter plot con la division de grupos
# obtenidos (se utiliza la matriz de datos centrados).
col.cluster<-c("blue", "red", "green", "purple")[cl.kmeans]
pairs(X.s, col=col.cluster, main="k-means", pch=19)

#-----------------------------------------
#  Visualizacion con las dos componentes principales
#-----------------------------------------

library(cluster) 

clusplot(X.s, cl.kmeans, 
         main="Dos primeras componentes principales")

text(princomp(X.s)$score[,1:2],
     labels=rownames(X.s), pos=1, col="blue")

#-------------------------------------
#  Silhouette
#--------------------------------------
# Representacion grafica de la eficacia de
# clasificacion de una observacion dentro de un
# grupo.
# Mayor a 0.7 = mejor clasificación
# Entre más cercana a 1 es mejor.

# 1.- Generacion de los calculos
dist.Euc<-dist(X.s, method = "euclidean")
Sil.kmeans<-silhouette(cl.kmeans, dist.Euc)

#2.- Generacion del grafico
plot(Sil.kmeans, main="Silhouette for k-means", 
     col="blue")

### Con 4 clusters, hay 2 datos no clasificados.
### El ancho del Silhouette es de 0.29.
### Hay dos clusters con una probabilidad del Silhouette buena y dos clusters
### con probabilidad un poco baja.

### La mejor clasificación, la que favorece por el valor del ancho del Silhouette y 
### por sus valores de probabilidad del Silhouette, sería un clúster de 2 grupos.
### Esto se debe a que, el ancho del Silhouette es 0.3 y es mayor que del clúster 3 y 4.
### Además de que la probabilidad de los dos clúster es buena.
