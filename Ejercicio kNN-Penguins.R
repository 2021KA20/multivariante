#-------------------------------
# EJERCICIO PENGUINS
#-------------------------------

#1.- Descargar la matriz penguins
#2.- Copiar y pegar el script
#3.- Adaptar el script (semillas particulares).
#4.- Generar resultados (activar comandos)
#5.- Responder las siguientes preguntas:
#5.1.- ¿Cuál es el número óptimo de K-vecinos cercanos?
#5.2.- ¿Cuál es la cantidad de observaciones mal clasificadas?
#5.3.- ¿Cuál es el ratio de mala clasificación (MR)?
#6.- Generar el gráfico de buena y mala clasificación.


#______ kNN_______
#K-vecinos próximos

# Cargar los datos penguins
library(readxl)
penguins <- read_excel("C:/Users/TOSHIBA/Downloads/penguins.xlsx")
head(penguins)
Z<-as.data.frame(penguins)

# Definir la matriz de datos y la variable respuesta
# Con las clasificaciones
x<-Z[,4:7] #variable cuantitativa
y<-Z[,2] #variable cualitativa (especie)
y <- as.factor(y) #se convierte en factor la variable cualitativa (y)

# Se definen las variables y observaciones
n<-nrow(x)
p<-ncol(x)

# Gráfico scatter plot
# Creación de un vector de colores
col.penguins <- c("red","green", "blue")[y]
col.penguins

pairs(x, main="Data set Penguins", pch=17,col=col.penguins)

#-----------------------
#         kNN
#-----------------------

library(class)

# Se fija una "semilla" para tener valores iguales
set.seed(12345)

# Creación de los ciclos
# para k=1 hasta k=20
# Selecciona el valor de k que tenga el error mas bajo.
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

knn.tables[[1]]

# El más eficiente es k=1
# Se señala el k más eficiente
k.opt<-1

knn.cv.opt<-knn.class[[k.opt]]
knn.cv.opt

# Tabla de contingencia con las clasificaciones buenas y malas
knn.tables[[k.opt]]

# Cantidad de observaciones mal clasificadas
knn.mis[k.opt]

# Error de clasificación (MR)
knn.mis[k.opt]/n

## Responder las siguientes preguntas:
# ¿Cuál es el número óptimo de K-vecinos cercanos?
### El número óptimo de k-vecinos es 1.

# ¿Cuál es la cantidad de observaciones mal clasificadas?
### Las observaciones mal clasificadas son 44 de las 3 especies de pingüinos.

# ¿Cuál es el ratio de mala clasificación (MR)?
### El ratio de mala clasificación es de 0.127907.

# Gráfico de clasificaciones correctas y erróneas
col.knn.penguins<-c("indianred1","black")[1*(y==knn.cv.opt)+1]
pairs(x, main="Clasificación kNN de Penguins",
      pch=19, col=col.knn.penguins)

#Las malas clasificaciones están marcadas en rojo y las buenas en negro.

