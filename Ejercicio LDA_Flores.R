
#________ ANALISIS DISCRIMINANTE LINEAL_____

library(MASS)

# Se cargan los datos iris
Z<-as.data.frame(iris)
colnames(Z)
z1 <- Z[71,]
z1

# Se define la matriz de datos y la variable
# respuesta con las clasificaciones.
x<-Z[,1:4] #selecci�n de variables cuantitativas
y<-Z[,5] #selecci�n de la variable categ�rica (respuesta)

# Definir como n y p el n�mero de flores y
# variables
n<-nrow(x)
p<-ncol(x)

# Se aplica el An�lisis discriminante lineal (LDA)
# Cross validation (cv): clasificaci�n optima
lda.iris<-lda(y~.,data=x, CV=TRUE)

# lda.iris$class contiene las clasificaciones hechas
# por CV usando LDA.
lda.iris$class

# Creaci�n de la tabla de clasificaciones buenas y malas
table.lda<-table(y,lda.iris$class)
table.lda

# Proporci�n de errores
mis.lda<- n-sum(y==lda.iris$class)
mis.lda/n #por cada 100 flores que se clasifique se equivocar� dos veces.


# scater plot
# Buenas clasificaciones en negro y malas en rojo
col.lda.iris<-c("indianred1","black")[1*(y==lda.iris$class)+1]
pairs(x,main="Buena clasificaci�n (negro), mala clasificaci�n (rojo)",
      pch=19,col=col.lda.iris)

# Probabilidad de pertenencia a uno de los tres grupos
lda.iris$posterior 

# Gr�fico de probabilidades
plot(1:n, lda.iris$posterior[,1],
     main="Probabilidades a posteriori",
     pch=20, col="blue",
     xlab="N�mero de observaciones", ylab="Probabilidades")
points(1:n,lda.iris$posterior[,2],
       pch=20, col="green")
points(1:n,lda.iris$posterior[,3],
       pch=20, col="orange")