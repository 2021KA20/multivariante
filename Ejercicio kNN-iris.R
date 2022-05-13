
#______ kNN_______
#K-vecinos próximos

library(MASS)

# Cargar los datos iris

Z<-as.data.frame(iris)

# Definir la matriz de datos y la variable respuesta
# Con las clasificaciones
x<-Z[,1:4] #variable cuantitativa
y<-Z[,5] #variable cualitativa

# Se definen las variables y observaciones
n<-nrow(x)
p<-ncol(x)

# Gráfico scatter plot
# Creación de un vector de colores
col.iris<-c("blue","green","orange")[y]
col.iris

pairs(x, main="Data set Iris", pch=19,col=col.iris)

#-----------------------
#         kNN
#-----------------------

library(class)

# Se fija una "semilla" para tener valores iguales
set.seed(1000)

# Creación de los ciclos
# para k=1 hasta k=20
# Selecciona el valor de k que tenga el error
# mas bajo.
# Inicialización de una lista vacía de tamaño 20
knn.class<-vector(mode="list",length=20)
knn.tables<-vector(mode="list", length=20)

# Clasificaciones erróneas
knn.mis<-matrix(NA, nrow=20, ncol=1)
knn.mis

for(k in 1:20){
  knn.class[[k]]<-knn.cv(x,y,k=k)
  knn.tables[[k]]<-table(y,knn.class[[k]])
  # la suma de las clasificaciones menos las correctas
  knn.mis[k]<- n-sum(y==knn.class[[k]])
}

knn.mis

# Número óptimo de k-vecinos
which(knn.mis==min(knn.mis))

knn.tables[[10]]
knn.tables[[18]]
knn.tables[[14]]
knn.tables[[19]]

# El más eficiente es k=14
# Se señala el k más eficiente
k.opt<-14

knn.cv.opt<-knn.class[[k.opt]]
knn.cv.opt

# Tabla de contingencia con las clasificaciones buenas y malas
knn.tables[[k.opt]]

# Cantidad de observaciones mal clasificadas
knn.mis[k.opt]

# Error de clasificación (MR)
knn.mis[k.opt]/n

# Gráfico de clasificaciones correctas y erróneas
col.knn.iris<-c("indianred1","black")[1*(y==knn.cv.opt)+1]
pairs(x, main="Clasificación kNN de Iris",
      pch=19, col=col.knn.iris)




