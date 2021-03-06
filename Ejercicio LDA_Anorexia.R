#________ ANALISIS DISCRIMINANTE LINEAL_____

library(MASS)

# Se cargan los datos de anorexia
Z<-as.data.frame(anorexia)
colnames(Z)
str(Z)
anyNA(Z)

# Se define la matriz de datos y la variable
# respuesta con las clasificaciones.
x<-Z[,2:3] #selecci�n de variables cuantitativas
y<-Z[,1] #selecci�n de la variable categ�rica (respuesta)

# Definir como n y p el n�mero de pacientes y
# variables
n<-nrow(x)
p<-ncol(x)

# Se aplica el An�lisis discriminante lineal (LDA)
# Cross validation (cv): clasificaci�n �ptima
lda.anorexia<-lda(y~.,data=x, CV=TRUE)

# lda.anorexia$class contiene las clasificaciones hechas
# por CV usando LDA.
lda.anorexia$class

# Creaci�n de la tabla de clasificaciones buenas y malas
table.lda<-table(y,lda.anorexia$class)
table.lda

# Proporci�n de errores
mis.lda<- n-sum(y==lda.anorexia$class)
mis.lda/n #por cada 100 pacientes que se clasifique se equivocar� 52 veces.


# scater plot
# Buenas clasificaciones en negro y malas en rojo
col.lda.anorexia<-c("indianred1","black")[1*(y==lda.anorexia$class)+1]
pairs(x,main="Buena clasificaci�n (negro), mala clasificaci�n (rojo)",
      pch=19,col=col.lda.anorexia)

# Probabilidad de pertenencia a uno de los tres grupos
lda.anorexia$posterior 

# Gr�fico de probabilidades
plot(1:n, lda.anorexia$posterior[,1],
     main="Probabilidades a posteriori",
     pch=20, col="red",
     xlab="N�mero de observaciones", ylab="Probabilidades")
points(1:n,lda.anorexia$posterior[,2],
       pch=20, col="green")
points(1:n,lda.anorexia$posterior[,3],
       pch=20, col="blue")
