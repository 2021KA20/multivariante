#________ ANALISIS DISCRIMINANTE LINEAL_____

library(MASS)

# Se cargan los datos de anorexia
Z<-as.data.frame(anorexia)
colnames(Z)
str(Z)
anyNA(Z)

# Se define la matriz de datos y la variable
# respuesta con las clasificaciones.
x<-Z[,2:3] #selección de variables cuantitativas
y<-Z[,1] #selección de la variable categórica (respuesta)

# Definir como n y p el número de pacientes y
# variables
n<-nrow(x)
p<-ncol(x)

# Se aplica el Análisis discriminante lineal (LDA)
# Cross validation (cv): clasificación óptima
lda.anorexia<-lda(y~.,data=x, CV=TRUE)

# lda.anorexia$class contiene las clasificaciones hechas
# por CV usando LDA.
lda.anorexia$class

# Creación de la tabla de clasificaciones buenas y malas
table.lda<-table(y,lda.anorexia$class)
table.lda

# Proporción de errores
mis.lda<- n-sum(y==lda.anorexia$class)
mis.lda/n #por cada 100 pacientes que se clasifique se equivocará 52 veces.


# scater plot
# Buenas clasificaciones en negro y malas en rojo
col.lda.anorexia<-c("indianred1","black")[1*(y==lda.anorexia$class)+1]
pairs(x,main="Buena clasificación (negro), mala clasificación (rojo)",
      pch=19,col=col.lda.anorexia)

# Probabilidad de pertenencia a uno de los tres grupos
lda.anorexia$posterior 

# Gráfico de probabilidades
plot(1:n, lda.anorexia$posterior[,1],
     main="Probabilidades a posteriori",
     pch=20, col="red",
     xlab="Número de observaciones", ylab="Probabilidades")
points(1:n,lda.anorexia$posterior[,2],
       pch=20, col="green")
points(1:n,lda.anorexia$posterior[,3],
       pch=20, col="blue")
