mc <-mtcars

dim(mc)

##################################################################
##################Contar hp (dados duplicados)####################
#nrow(mc$hp[duplicated(mc$hp), ])
hpdata <- data.frame(mc$hp)
head(hpdata)
dim(hpdata)
hpdata
#Deleting duplicated data in a dataframe
#hpdata <- hpdata[!duplicated(hpdata), ]

#Eliminating duplicateds information
unique(hpdata)

#Duplicated vector
duplicated(hpdata)

#Select repeated elements in one or more columns
distinct(hpdata) # If there are more column is necessary specify like distinct(data, column)

#Count duplicateds -->> Doesnt work
#nrow(hpdata[duplicated(hpdata), ])
#hpdata[duplicated(hpdata), ]

table(hpdata)
as.data.frame(table(hpdata))

#Show duplicateds
hpdata[duplicated(hpdata), ]

t(hpdata)
####################fim do test####################
###################################################

hist(mc$hp)
density(mc$hp)
plot(density(mc$hp))
barplot(mc$hp)
pie(table(mc$hp))
cor(mc$hp, mc$qsec)
cor(mc$hp, mc$mpg)
cor(mc$hp, mc$wt)

boxplot(mc$hp)
pairs(mc)

dotchart(mc$hp)
sunflowerplot(mc$hp, mc$qsec)
library(lattice)
stripplot(mc$hp) 

library(lattice)
#attach(scanvote)
coplot(mc$hp ~ mc$cyl | mc$qsec)


#attach(sumcr)
coplot(mc$hp ~ mc$cyl | mc$qsec, pch=15+as.integer(mc$wt), cex=1.5,
       number=5, columns=5,
       panel=function(x,y,...) {
         panel.smooth(x,y,span=.8,iter=10,...)
         abline(lm(y ~ x), col="blue")
       } )


coplot(mc$hp ~ log10(mc$cyl) | mc$qsec, columns=3,
       panel=function(x,y,...) {
         panel.smooth(x,y,span=.8,iter=5,...)
         abline(lm(y ~ x), col="blue") } )

log(mc$hp,6)

kmeans(mc$hp, 5, iter.max = 10, nstart = 1,algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), trace=FALSE)

dt <- dist(mc$hp, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
dt
hc <- hclust(dt, method = "average", members = NULL) 
plot(hc)

install.packages('fpc') 
install.packages('kernlab') 
library(fpc)
dbscan(mc$hp, 3, MinPts = 2, scale = FALSE,
       method = c("hybrid","raw","dist"), seeds = TRUE, showplot = FALSE, countmode = NULL)

install.packages('lattice')
install.packages('plotly')
library(plotly)
library(lattice)

contourplot()

cars <- mc
cars$hp <- NULL

(kc <- kmeans(cars, 3, iter.max = 1000))

table(mc$hp, kc$cluster)

dt <- dist(mc$hp, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
hc <- hclust(dt, method = "average", members = NULL) 
plot(hc)

c <-mtcars
(kc <- kmeans(c,3))
kc
kc$cluster

table(kc$cluster)
