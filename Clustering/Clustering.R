#3
#a
vec <- c(2,3,4,5,7,9,11)
iris<-iris
iris_copy <- scale(subset(iris, select=c(1:4)))
iris_copy2 <- subset(iris, select=c(1:4))
km_result <- list()
for(i in vec) {
  km_result[[i]] <- kmeans(iris_copy,i)
}
for (i in vec){
  print(paste("Size of clusters when n = ", i))
  print(assign(paste("size",i,sep=""),km_result[[i]]$size))
}

cluster.result<-list()
for (i in vec){
  
  cluster.result[[i]]<-as.vector(km_result[[i]]$cluster)
}

a<-function(cluster){
  if (is.null(cluster)==T){
    return(1)
  }else{
    a<-0
    for(i in 1:nrow(iris)){
      for (j in 1:nrow(iris)){
        if (cluster[i]==cluster[j] & iris$Species[i]==iris$Species[j]){
          a<-a+1
        }
      }
    }
    if(a>=1){
      a<-a-1
    }else{
      a<-a
    }
    return(a)
  }
}

b<-function(cluster){
  if (is.null(cluster)==T){
    return(1)
  }else{
    a<-0
    for(i in 1:nrow(iris)){
      for (j in 1:nrow(iris)){
        if (cluster[i]!=cluster[j] & iris$Species[i]!=iris$Species[j]){
          a<-a+1
        }
      }
    }
    if(a>=1){
      a<-a-1
    }else{
      a<-a
    }
    return(a)
  }
}
c<-function(cluster){
  if (is.null(cluster)==T){
    return(1)
  }else{
    a<-0
    for(i in 1:nrow(iris)){
      for (j in 1:nrow(iris)){
        if (cluster[i]==cluster[j] & iris$Species[i]!=iris$Species[j]){
          a<-a+1
        }
      }
    }
    if(a>=1){
      a<-a-1
    }else{
      a<-a
    }
    return(a)
  }
}
d<-function(cluster){
  if (is.null(cluster)==T){
    return(1)
  }else{
    a<-0
    for(i in 1:nrow(iris)){
      for (j in 1:nrow(iris)){
        if (cluster[i]!=cluster[j] & iris$Species[i]==iris$Species[j]){
          a<-a+1
        }
      }
    }
    if(a>=1){
      a<-a-1
    }else{
      a<-a
    }
    return(a)
  }
}
a.list<-list()
b.list<-list()
c.list<-list()
d.list<-list()

for (i in vec){
  a.list[[i]]<-a(cluster.result[[i]])
  b.list[[i]]<-b(cluster.result[[i]])
  c.list[[i]]<-c(cluster.result[[i]])
  d.list[[i]]<-d(cluster.result[[i]])
}

#b
#Values of the F-measures for clusters 2,3,4,5,7,9,11
#patient, this loop takes a while "like 1 minute" to complete
for (i in vec){
  print(paste("#F measure for cluster size = ", i))
  print(assign(paste("f",i,sep=""),
               (2*(a.list[[i]]/(a.list[[i]]+c.list[[i]]))*
                  (a.list[[i]]/(a.list[[i]]+d.list[[i]])))/
                 (a.list[[i]]/(a.list[[i]]+c.list[[i]])+a.list[[i]]/(a.list[[i]]+d.list[[i]]))))
}
l<-list(f2,f3,f4,f5,f7,f9,f11)

#c
#Cluster k=3 has highest F measure.
print("#F measure for cluster size = 3")
f3

#d
#The highest F measure correlates with the optimal cluster size. This was true in this case, and would be interesting to try additional cases.


#4
#HAC report, including the heights of the distance and the dendrogram
iris_dist <- dist(iris_copy)
new<-hclust(iris_dist)
new[[2]]
plot(new)
postscript("one.eps")
plot(new)
dev.off()

#b
#after eyeballing the cluster dendrogram, we see that the branching gets disipersed and complicated
#our only hope to accurately cluster them then might be to take our clusters from the early stages of
#the branching, because branches at lv. 2 show a distinct difference in height, we should grab our lv. 3
#branches which show more similar heights and use those lv. 3 nodes as our initial clusters.

#c
#cluster number and composition for each cluster with kmeans for cluster 1,2,3
#c

x<-as.vector(km_result[[3]]$cluster)
kmeans<-as.factor(x)

iris<-cbind(iris,kmeans)

#cluster number and composition for each cluster with kmeans for cluster 1,2,3
kmeans1<-which(x==1)
#composition and number of data points for cluster 1
kmeans1
length(kmeans1)
#composition and number of data points for cluster 2
kmeans2<-which(x==2)
kmeans2
length(kmeans2)
#composition and number of data points for cluster 3
kmeans3<-which(x==3)
kmeans3
length(kmeans3)

library(ggplot2)
df=iris
m=as.matrix(cbind(df$Petal.Length, df$Petal.Width),ncol=2)

cl=(kmeans(m,3))

cl$size
cl$withinss

df$cluster=factor(cl$cluster)
centers=as.data.frame(cl$centers)

ggplot(data=df, aes(x=Petal.Length, y=Petal.Width, color=cluster )) + 
  geom_point() + 
  geom_point(data=centers, aes(x=V1,y=V2, color="Center")) +
  geom_point(data=centers, aes(x=V1,y=V2, color="Center"), size=52, alpha=.3, legend=FALSE)

#The number of data points in each cluster is fairly different between the
#2 methods for all 3 clusters.The composition of each cluster is fairly similar for both methods.
#The differences can be noted on the 2 graphs above


#5
library(dbscan)
iris_mat<- as.matrix(iris_copy2)
iris_mat_result<-list()
eps <- c(0.2, 0.3, 0.4, 0.5, 0.6, 0.8, 1.0)#TODO: broken
for (i in eps) {
  iris_mat_result[[i]] <- dbscan(iris_mat, eps = eps[i])
}

#b
#Size of clusters
#Count number of clusters for each differing epsilon value
null<-numeric()
for (i in 1:7){
  null[i]<-length(which(iris_mat_result[[i]]$cluster==0))
}
#which(null==0)
#For epsilon =1, there is no noise data point
n<-numeric()
for (i in 1:7){
  n[i]<-length(unique(iris_mat_result[[i]]$cluster))
}
#Number of clusters for epsilon = 0.2, 0.3, 0.4, 0.5, 0.6, 0.8 or 1
#n-1
#Note: for epsilon=1, the number of clusters is 2 instead of 1
#size of clusters for differing epsilon values
#epsilon = .2
length(which(iris_mat_result[[1]]$cluster==1))
length(which(iris_mat_result[[1]]$cluster==2))
#epsilon = .3
length(which(iris_mat_result[[2]]$cluster==1))
length(which(iris_mat_result[[2]]$cluster==2))
length(which(iris_mat_result[[2]]$cluster==3))
#epsilon = .4
length(which(iris_mat_result[[3]]$cluster==1))
length(which(iris_mat_result[[3]]$cluster==2))
length(which(iris_mat_result[[3]]$cluster==3))
length(which(iris_mat_result[[3]]$cluster==4))
#epsilon = .5
length(which(iris_mat_result[[4]]$cluster==1))
length(which(iris_mat_result[[4]]$cluster==2))
#epsilon = .6
length(which(iris_mat_result[[5]]$cluster==1))
length(which(iris_mat_result[[5]]$cluster==2))
#epsilon = .8
length(which(iris_mat_result[[6]]$cluster==1))
length(which(iris_mat_result[[6]]$cluster==2))
#epsilon = 1
length(which(iris_mat_result[[7]]$cluster==1))
length(which(iris_mat_result[[7]]$cluster==2))
cluster.result2<-list()

for (i in 1:7){
  cluster.result2[[i]]<-as.numeric(iris_mat_result[[i]]$cluster)
}

a.list2<-list()
b.list2<-list()
c.list2<-list()
d.list2<-list()
for (i in 1:7){
  a.list2[[i]]<-a(cluster.result2[[i]])
  b.list2[[i]]<-b(cluster.result2[[i]])
  c.list2[[i]]<-c(cluster.result2[[i]])
  d.list2[[i]]<-d(cluster.result2[[i]])
}

#F-measure for epsilon = .2, .3, .4, .5, .6, .8, 1 in that order. The last value is the highest F-measure
for (i in 1:7){
  print(paste("#F measure for epsilon = .", i))
  print(assign(paste("f_mat",i,sep=""),
               (2*(a.list2[[i]]/(a.list2[[i]]+c.list2[[i]]))*
                  (a.list2[[i]]/(a.list2[[i]]+d.list2[[i]])))/
                 (a.list2[[i]]/(a.list2[[i]]+c.list2[[i]])+a.list2[[i]]/(a.list2[[i]]+d.list2[[i]]))))
}

#c
l2<-list(f_mat1,f_mat2,f_mat3,f_mat4,f_mat5,f_mat6,f_mat7)
#which.max(l2)
f_mat7
#F-measure is greatest for epsilon = 1

#d
#When epsilon =1, there is no more noise in the clustering. It appears that as epsilon increases,
#the amount of noise data points decreases. The F-measure is also the greatest when epsilon=1.

#e
#cluster composition for each cluster with kmeans
kmeans1<-which(x==1)

#composition and number of data points for cluster 1
kmeans1
length(kmeans1)
#composition and number of data points for cluster 2
kmeans2<-which(x==2)
kmeans2
length(kmeans2)
#composition and number of data points for cluster 3
kmeans3<-which(x==3)
kmeans3
length(kmeans3)

#cluster composition with HAC
#composition and number of data points for cluster 1

length(hac1)
hac1
#composition and number of data points for cluster 2

length(hac2)
hac2
#composition and number of data points for cluster 3

length(hac3)
hac3

#Cluster composition with dbscan
#There are only 2 clusters
#Size and composition of data points for cluster 1 and 2
#Size and composition of data points for cluster 1
which(iris_mat_result[[7]]$cluster==1)
length(which(iris_mat_result[[7]]$cluster==1))
#Size and composition of data points for cluster 2
which(iris_mat_result[[7]]$cluster==2)
length(which(iris_mat_result[[7]]$cluster==2))
#dbscan method produces a clustering with only 2 clusters, different from kmeans or hac method.
#The number of data points in each cluster is fairly different amongst the
#3 methods. It is rather difficult to compare the composition of clusters in dbscan method with the
#other two methods because they don't have the same number of clusters.

#6
swiss<-swiss
swiss_copy <- scale(swiss)
swiss_dist <- dist(swiss_copy)
fit<-hclust(swiss_dist)

hacswiss1<-as.vector(t(read.table("catholic.txt",sep=",")))
hacswiss2<-as.vector(t(read.table("protestant.txt",sep=",")))

hacswiss<-cbind(hacswiss1,1)
hacswiss.2<-cbind(hacswiss2,2)
hacswiss<-rbind(hacswiss,hacswiss.2)
hacswiss <- hacswiss[order(hacswiss[,1]),]
hac.factor<-as.factor(hacswiss[,2])
swiss<-cbind(swiss,hac.factor)
library(car)
scatterplot(Catholic~Examination|hac.factor,data=swiss,smoother=NULL,reg.line=NULL,main="HAC")

#Catholic-predominant cities
rownames(swiss[hacswiss1,])
#Protestant-predominant cities
rownames(swiss[hacswiss2,])