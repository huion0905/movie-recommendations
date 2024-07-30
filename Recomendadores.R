#MOVIE LENS MATRIZ SPARSE eficiente en el guardado de datos
#
#arma un dataframe
#columna y fila solamente las posiciones con valores > 0

#Visualización de la Realmatrix
load("Movilense.RData")
library(recommenderlab)
data("MovieLense")

image(MovieLense)

rating_movies<-MovieLense[rowCounts(MovieLense)>50,
                          colCounts(MovieLense)>100]

image(rating_movies)

#forma 1
t.id<-sample(c(TRUE,FALSE),size = nrow(rating_movies),
             replace = TRUE,prob = c(0.8,0.2))

#forma 2
library(caret)
filas<-1:nrow(rating_movies)
training_id<-createDataPartition(filas,p = 0.7,list = FALSE)

train<-rating_movies[t.id,]
test<-rating_movies[!t.id,]
nrow(train)
nrow(test)

#FILTRADO COLABORATIVO BASADO EN LOS ITEMS(PELICULAS)

#Calcula las similitudes entre dos items cualesquiera
#sabiendo que tan parecidos son dos items lo va a poder
#recomendar

#Ej La guerra de las galaxias (5)  user Ricardo
#El imperio Contraataca---> Me lo recomienda
#item based Collaborative Filter

#Contruyo el objeto
ibcf<-Recommender(data = train,method="IBCF",
                  parameter=list(k=30))

ibcf.mod<-getModel(ibcf)
ibcf.mod

n_rec<-5

ibcf.pred<-predict(object = ibcf,newdata=test,n=n_rec)
ibcf.pred

ibcf.rec.matrix<-sapply(X = ibcf.pred@items, function(x){
  colnames(rating_movies)[x]})

View(ibcf.rec.matrix[,1:3])

#FILTRADO COLABORATIVO BASADO EN LOS USER(USUARIOS)
#Quien se parece a Ricardo, Valery han visto peliculas
#similares, que peliculas ha visto Valery y que Ricardo no

#Contruyo el objeto
ubcf<-Recommender(data = train,method="UBCF")

ubcf.mod<-getModel(ubcf)
ubcf.mod

n_rec<-5

ubcf.pred<-predict(object = ubcf,newdata=test,n=n_rec)
ubcf.pred

ubcf.rec.matrix<-sapply(X = ubcf.pred@items, function(x){
  colnames(rating_movies)[x]})

View(ubcf.rec.matrix[,1:3])
