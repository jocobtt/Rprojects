setwd('~/Notebooks')
#load packages
library(datasets)
library(stats)
library(dbscan)
library(dplyr)
library(factoextra)
#load iris dataset
data(iris)
head(iris)

#copy w/out target variable 
iris_cop <- subset(iris,select=c(1:4))
#run kmeans for k=2,3,4,5,7,9,11 and report size of clusters and f-measure
wit_two <- kmeans(iris_cop,2)
str(wit_two)
fviz_cluster(wit_two,data=iris_cop,geom='point')
#compute f-score
#adjust this function - will this work? 
pseudoF = function(X, k){
  nk = length(k)
  n = nrow(X)
  T = sum(scale(X,scale=F)^2)
  W = rep(T, nk)
  for (i in 1:nk){
    cli = kmeans(X, k[i])
    W[i] = sum(cli$withinss)
  }
  pF = ((T-W)/(k-1))/(W/(n-k))
  return(list(k=k, W=W, pF=pF))
}

ir <- pseudoF(scale(iris_cop),2:11)[3]
ir <- as.numeric(unlist(ir))
ir
which.max(ir)
max(ir)

#from our F value we can conclude that our best method is a k means with 2 means.

#comment about anything intersting about your experiment
#even though there are 3 categories it fits best when we have 2 means not 3. 


#4 run hierarchical agglomerative clustering algorithm 
dist_iris <- dist(x = iris_cop)
h_clust_val <- hclust(dist_iris)

#graph dendrogram of hcluster algorithm
postscript("")#where to save plot-postscript file
plot(h_clust_val)
dev.off()
#select optimal threshold 
#compare to kmeans clustering

#5 dbscan algorithm 
#turn iris x's into matrix 
matr_iris <- as.matrix(iris_cop)
#run dbscan for espilon= .2,.3,.4,.5,.6,.8,1
espp <- c(.2,.3,.4,.5,.6,.8)
run <- lapply(espp,function(x) dbscan(matr_iris,x))

varr <- dbscan(matr_iris,.2)
fviz_cluster(dbscan(matr_iris,.2),data=matr_iris,geom='point')

#find optimal epsilon value
dbscan::kNNdistplot(matr_iris,k=4)
abline(h=.6,col='red')
rrr <- dbscan::dbscan(matr_iris,0.6,5)
fviz_cluster(rrr,matr_iris,geom='point')

#report epsilon value that produces best F-score 
#.6 is the best epsilon value that produces the best F-score
#comment on anything interesting about your experiment 
#how does corresponding number and nature of clusters compare with k means and hierarchical clustering?
length(rrr$cluster)
#6 pick best clustering algorithm for swiss dataset 
#produce a list of the swiss cities predominantly protestant and those mainly catholic 
data("swiss")
View(swiss)
#run dbscan 
swiss_sub <- subset(swiss,select=5)
swiss_mat <- as.matrix(swiss_sub)
dbscan::kNNdistplot(swiss_mat,k=2)
dbscan::kNNdistplot(swiss_mat,k=2)
abline(h=(2),col='red')
swis_deb <- dbscan::dbscan(swiss_mat,2,2)

dbscan(swiss_mat,3,2)
fviz_cluster(swis_deb,data=swiss,geom='point')

