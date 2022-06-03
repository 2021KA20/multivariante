
#______ Escalado Multidimensional Métrico______

# Cargamos la matriz de datos eurodist
data.dist<-eurodist
data.dist

# Transformamos los datos en matriz
data.dist<-as.matrix(data.dist)

dim(data.dist) #Dimensión
View(data.dist)
str(data.dist) #Tipo de datos
colnames(data.dist) #Nombre de las columnas

#-----------------------------------
#  Extracción de las filas de la matriz
#-----------------------------------

# Número de ciudades
n<-nrow(data.dist)

#------------------------------------
#  Escalado Multidimensional Clásico
#------------------------------------
# 1.- Cálculo de autovalores
# Dentro del objeto mds.cities se encuentran
# almacenado los valores propios (eigenvalues) en
# mds.cities$eig

mds.cities<-cmdscale(data.dist, eig = TRUE)

# 2.- Generación del gráfico
plot(mds.cities$eig, pch=19, col="blue", 
     xlab="Números", ylab="Valores Propios",
     type="o")
abline(a=0, b=0, col="red")

# Interpretación: se identifican autovalores negativos
# Se considera como solución el seleccionar
# r=2 coordenadas principales.


# 3.- Medidas de precisión
m<-sum(abs(mds.cities$eig[1:2]))/sum(abs(mds.cities$eig))

#4.- Obtención de coordenadas principales fijando
# k=2 y que se realice con los dos primeros autovalores.
mds.cities<-cmdscale(data.dist, eig=TRUE, k=2)

x1<-mds.cities$points[,1]
x2<-mds.cities$points[,2]

# 5.- Generación del gráfico en dos dimensiones de los
# datos con las coordenadas obtenidas
plot(x1,x2,pch=19, col="blue", 
     xlim = range(x1)+c(0,600))
text(x1,x2, pos=4, labels = rownames(data.dist),
     col="black")

# Se invierten los ejes del plot
x2<--x2

plot<-plot(x1,x2,pch=19, col="purple", 
     xlim = range(x1)+c(0,600))
text(x1,x2, pos=4, labels = rownames(data.dist),
     col="black")
