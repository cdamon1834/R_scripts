getwd()
cars = read.csv('cars.csv',stringsAsFactors=FALSE)
head(cars)
cars.use = cars[,-c(1,2)]
medians = apply(cars.use,2,median)
mads = apply(cars.use,2,mad)
cars.use = scale(cars.use,center=medians,scale=mads)
cars.dist = dist(cars.use)
cars.hclust = hclust(cars.dist)
cars.dist
cars.hclust = hclust(cars.dist)
plot(cars.hclust,labels=cars$Car,main='Default from hclust')
groups.3 = cutree(cars.hclust,3)
table(groups.3)
counts = sapply(2:6,function(ncl)table(cutree(cars.hclust,ncl)))
names(counts) = 2:6
counts
cars$Car[groups.3 == 1]
sapply(unique(groups.3),function(g)cars$Car[groups.3 == g])
groups.4 = cutree(cars.hclust,4)
sapply(unique(groups.4),function(g)cars$Car[groups.4 == g])
table(groups.3,cars$Country)
aggregate(cars.use,list(groups.3),median)
aggregate(cars[,-c(1,2)],list(groups.3),median)
a3 = aggregate(cars[,-c(1,2)],list(groups.3),median)
data.frame(Cluster=a3[,1],Freq=as.vector(table(groups.3)),a3[,-1])
a4 = aggregate(cars[,-c(1,2)],list(groups.4),median)
data.frame(Cluster=a4[,1],Freq=as.vector(table(groups.4)),a4[,-1])
