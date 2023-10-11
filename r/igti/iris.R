i <-iris
dim(i)  

hist(i$Sepal.Length) 
density(i$Sepal.Length)
plot(density(i$Sepal.Length))
barplot(i$Sepal.Length)
pie(table(i$Species))
cor(i$Sepal.Length, i$Petal.Length)
cor(i$Sepal.Length, i$Petal.Length) #Duplicado
cor(i$Petal.Width, i$Sepal.Width)

boxplot(i$Sepal.Length)
pairs(i)

kmeans(i$Sepal.Length, 5, iter.max = 10, nstart = 1,algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), trace=FALSE)

d <- dist(i$Petal.Width, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
d
e <- hclust(d, method = "average", members = NULL) 
plot(e)

install.packages('fpc') 
install.packages('kernlab') 
library(fpc)
dbscan(i$Sepal.Length, 3, MinPts = 2, scale = FALSE,
       method = c("hybrid","raw","dist"), seeds = TRUE, showplot = FALSE, countmode = NULL)

contourplot()


newiris <- i
newiris$Species <- NULL

(kc <- kmeans(newiris, 3))

table(i$Species, kc$cluster)

#c <-mtcars
#(kc <- kmeans(c,3))

kc$cluster

table(kc$cluster)
