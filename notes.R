library(ISLR)
library(ggplot2)

head(iris)


# color by predicted factor
pl <- ggplot(iris, aes(Petal.Length,Petal.Width, color=Species))
print(pl + geom_point(size=3))

# k means algorithm
set.seed(101)

irisCluster <- kmeans(iris[,1:4],centers = 3, nstart = 20)
print(irisCluster)

# only can do this because we have the labels to compare
table(irisCluster$cluster,iris$Species)

library(cluster)
clusplot(iris,irisCluster$cluster, color = T, shade = T, labels = 0, lines = 0)
