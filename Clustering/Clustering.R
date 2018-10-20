###############################
##### BYU Big Data Science ####
### Lab 7 - Clustering in R ###
###############################
## A. Anderson, J. Campbell, ##
### A. Hamilton, C Timpson ####
###############################

set.seed(20)

library(stats)
library(ggplot2)
library(dbscan)
library(datasets)
library(car)

##################
### Question 3 ###
##################

## Part A ##

vec <- c(2, 3, 4, 5, 7, 9, 11)
head(iris)
iris_copy <- subset(iris, select = c(1:4))

k_means_result <- list()
for(i in vec) {
  k_means_result[[i]] <- kmeans(iris_copy,i)
}

for (i in vec){
  print(paste("Size of clusters when n = ", i))
  print(assign(paste("size",i,sep=""), k_means_result[[i]]$size))
}

cluster.result <- list()
for (i in vec){
  cluster.result[[i]]<-as.vector(k_means_result[[i]]$cluster)
}

fun_a <- function(cluster){
  
  if (is.null(cluster) == TRUE){
    return(1)
  }else{
    a <- 0
    for(i in 1:nrow(iris)){
      for (j in 1:nrow(iris)){
        if (cluster[i] == cluster[j] & iris$Species[i] == iris$Species[j]){
          a <- a + 1
        }
      }
    }
    if(a >= 1){
      a <- a - 1
    }else{
      a <- a
    }
    return(a)
  }
  
}

fun_b <- function(cluster){
  
  if (is.null(cluster) == TRUE){
    return(1)
  }else{
    b <- 0
    for(i in 1:nrow(iris)){
      for (j in 1:nrow(iris)){
        if (cluster[i] != cluster[j] & iris$Species[i] != iris$Species[j]){
          b <- b + 1
        }
      }
    }
    if(b >= 1){
      b <- b - 1
    }else{
      b <- b
    }
    return(b)
  }
  
}

fun_c <- function(cluster){
  
  if(is.null(cluster) == TRUE){
    return(1)
  }else{
    c <- 0
    for(i in 1:nrow(iris)){
      for (j in 1:nrow(iris)){
        if (cluster[i] == cluster[j] & iris$Species[i] != iris$Species[j]){
          c <- c + 1
        }
      }
    }
    if(c >= 1){
      c<- c - 1
    }else{
      c <- c
    }
    return(c)
  }
  
}

fun_d <- function(cluster){
  
  if(is.null(cluster) == TRUE){
    return(1)
  }else{
    d <- 0
    for(i in 1:nrow(iris)){
      for (j in 1:nrow(iris)){
        if (cluster[i] != cluster[j] & iris$Species[i] == iris$Species[j]){
          d <- d + 1
        }
      }
    }
    if(d >= 1){
      d <- d - 1
    }else{
      d <- d
    }
    return(d)
  }
  
}


a.list <- list()
b.list <- list()
c.list <- list()
d.list <- list()

for (i in vec){
  a.list[[i]] <- fun_a(cluster.result[[i]])
  b.list[[i]] <- fun_b(cluster.result[[i]])
  c.list[[i]] <- fun_c(cluster.result[[i]])
  d.list[[i]] <- fun_d(cluster.result[[i]])
}

## Part B

# Values of the F-measures for clusters 2,3,4,5,7,9,11
# patient, this loop takes a while "like 1 minute" to complete

for (i in vec){
  print(paste("F-measure for k =", i))
  print(assign(paste("f", i, sep=""),
               (2*(a.list[[i]]/(a.list[[i]]+c.list[[i]]))*
                  (a.list[[i]]/(a.list[[i]]+d.list[[i]])))/
                 (a.list[[i]]/(a.list[[i]]+c.list[[i]])+a.list[[i]]/
                    (a.list[[i]]+d.list[[i]]))))
}

l <- list(f2, f3, f4, f5, f7, f9, f11)

## Part C

#Cluster k = 3 has highest F measure.
paste("k =", which.max(l) + 1,  "has the highest F-Measure:", l[which.max(l)])


## Part D

# The highest F measure correlates with the optimal cluster size. This was true 
# in this case, and would be interesting to try additional cases.



##################
### Question 4 ###
##################

## Part A

# HAC report, including the heights of the distance and the dendrogram
iris_dist <- dist(iris_copy)

new <- hclust(iris_dist)
plot(new)
x <- rect.hclust(new,k=3)
hac1 <- as.vector(x[[1]])
hac2 <- as.vector(x[[2]])
hac3 <- as.vector(x[[3]])

postscript("k3hclust.eps")
plot(new)
dev.off()


## Part B

# After eyeballing the cluster dendrogram, we see that the branching gets 
# disipersed and complicated our only hope to accurately cluster them, then
# might be to take our clusters from the early stages of the branching, because 
# branches at lv. 2 show a distinct difference in height, we should grab our lv. 3
# branches which show more similar heights and use those lv. 3 nodes as our initial 
# clusters.


## Part C

# cluster number and composition for each cluster with kmeans for cluster 1,2,3

x <- as.vector(k_means_result[[3]]$cluster)
kmeans <- as.factor(x)

iris_new <- cbind(iris_copy, kmeans)

hac <- cbind(hac1, 1)
hac.2 <- cbind(hac2, 2)
hac.3 <- cbind(hac3, 3)
hac <- rbind(hac, hac.2, hac.3)

hac <- hac[order(hac[,1]),]
hac.factor <- as.factor(hac[,2])
iris3 <- cbind(iris_new, hac.factor)

#cluster number and composition for each cluster with kmeans for cluster 1,2,3
kmeans1 <- which(x == 1)

#composition and number of data points for cluster 1
kmeans1
length(kmeans1)

#composition and number of data points for cluster 2
kmeans2 <- which(x == 2)
kmeans2
length(kmeans2)

#composition and number of data points for cluster 3
kmeans3 <- which(x == 3)
kmeans3
length(kmeans3)

clusters_matrix <- as.matrix(cbind(iris_new$Petal.Length, iris_new$Petal.Width),ncol=2)

cl <- (kmeans(clusters_matrix, 3))

cl$size
cl$withinss

iris_new$cluster <- factor(cl$cluster)
centers <- as.data.frame(cl$centers)

ggplot(data = iris_new, aes(x = Petal.Length, y = Petal.Width, color = cluster)) + 
  geom_point() + 
  geom_point(data = centers, aes(x = V1, y = V2, color = "Center")) +
  geom_point(data = centers, aes(x = V1, y = V2, color = "Center"), 
             size = 52, alpha = .3)

# The number of data points in each cluster is fairly different between the 2 
# methods for all 3 clusters.The composition of each cluster is fairly similar 
# for both methods. The differences can be noted on the graphs above



##################
### Question 5 ###
##################


## Part A

iris_mat <- as.matrix(iris_copy)
iris_matrix_result <- list()

eps <- c(.2, .3, .4, .5, .6, .8, 1)

for(i in 1:length(eps)){
  iris_matrix_result[[i]] <- dbscan(iris_mat, eps = eps[i])
}

## Part B

# Size of clusters
# Count number of clusters for each differing epsilon value
eps_diff <- numeric()
for (i in 1:length(eps)){
  eps_diff[i] <- length(which(iris_matrix_result[[i]]$cluster == 0))
}

which(eps_diff == 0)
# For epsilon = 1, there is no noise data point

n <- numeric()

for (i in 1:7){
  n[i]<-length(unique(iris_matrix_result[[i]]$cluster))
}

# Number of clusters for epsilon = 0.2, 0.3, 0.4, 0.5, 0.6, 0.8 or 1
# n-1
# Note: for epsilon=1, the number of clusters is 2 instead of 1
# size of clusters for differing epsilon values
# epsilon = .2
length(which(iris_matrix_result[[1]]$cluster==1))
length(which(iris_matrix_result[[1]]$cluster==2))
# epsilon = .3
length(which(iris_matrix_result[[2]]$cluster==1))
length(which(iris_matrix_result[[2]]$cluster==2))
length(which(iris_matrix_result[[2]]$cluster==3))
# epsilon = .4
length(which(iris_matrix_result[[3]]$cluster==1))
length(which(iris_matrix_result[[3]]$cluster==2))
length(which(iris_matrix_result[[3]]$cluster==3))
length(which(iris_matrix_result[[3]]$cluster==4))
# epsilon = .5
length(which(iris_matrix_result[[4]]$cluster==1))
length(which(iris_matrix_result[[4]]$cluster==2))
# epsilon = .6
length(which(iris_matrix_result[[5]]$cluster==1))
length(which(iris_matrix_result[[5]]$cluster==2))
# epsilon = .8
length(which(iris_matrix_result[[6]]$cluster==1))
length(which(iris_matrix_result[[6]]$cluster==2))
# epsilon = 1
length(which(iris_matrix_result[[7]]$cluster==1))
length(which(iris_matrix_result[[7]]$cluster==2))
cluster.result2 <- list()

for(i in 1:7){
  cluster.result2[[i]]<-as.numeric(iris_matrix_result[[i]]$cluster)
}

a.list2 <- list()
b.list2 <- list()
c.list2 <- list()
d.list2 <- list()

for (i in 1:7){
  a.list2[[i]] <- fun_a(cluster.result2[[i]])
  b.list2[[i]] <- fun_b(cluster.result2[[i]])
  c.list2[[i]] <- fun_c(cluster.result2[[i]])
  d.list2[[i]] <- fun_d(cluster.result2[[i]])
}

# F-measure for epsilon = .2, .3, .4, .5, .6, .8, 1 in that order. 
# The last value is the highest F-measure
for (i in 1:7){
  print(paste("F-measure for epsilon =", eps[i]))
  print(assign(paste("f_mat",i,sep=""),
               (2*(a.list2[[i]]/(a.list2[[i]]+c.list2[[i]]))*
                  (a.list2[[i]]/(a.list2[[i]]+d.list2[[i]])))/
                 (a.list2[[i]]/(a.list2[[i]]+c.list2[[i]])+a.list2[[i]]/
                    (a.list2[[i]]+d.list2[[i]]))))
}


## Part C

l2 <- list(f_mat1, f_mat2, f_mat3, f_mat4, f_mat5, f_mat6, f_mat7)

# F-measure is greatest for epsilon = 1
paste("espilon =", eps[which.max(l2)] ,"is where the greatest F-Measure:", l2[which.max(l2)])

## Part D

# When epsilon = 1, there is no more noise in the clustering. It appears that as
# epsilon increases, the amount of noise data points decreases. The F-measure 
# is also the greatest when epsilon=1.


## Part E
# cluster composition for each cluster with kmeans
kmeans1 <- which(x == 1)

# composition and number of data points for cluster 1
kmeans1
length(kmeans1)
# composition and number of data points for cluster 2
kmeans2 <- which(x == 2)
kmeans2
length(kmeans2)
# composition and number of data points for cluster 3
kmeans3 <- which(x == 3)
kmeans3
length(kmeans3)

# cluster composition with HAC
# composition and number of data points for cluster 1
hac1
length(hac1)

# composition and number of data points for cluster 2
hac2
length(hac2)

# composition and number of data points for cluster 3
hac3
length(hac3)


# Cluster composition with dbscan
# There are only 2 clusters
# Size and composition of data points for cluster 1 and 2
# Size and composition of data points for cluster 1
which(iris_matrix_result[[7]]$cluster==1)
length(which(iris_matrix_result[[7]]$cluster==1))
# Size and composition of data points for cluster 2
which(iris_matrix_result[[7]]$cluster==2)
length(which(iris_matrix_result[[7]]$cluster==2))
# dbscan method produces a clustering with only 2 clusters, different from kmeans or hac method.
# The number of data points in each cluster is fairly different amongst the
# 3 methods. It is rather difficult to compare the composition of clusters in dbscan method with the
# other two methods because they don't have the same number of clusters.



##################
### Question 6 ###
##################

swiss_copy <- scale(swiss)
swiss_dist <- dist(swiss_copy)
fit <- hclust(swiss_dist)

hacswiss1 <- as.vector(t(read.table("catholic.txt", sep=",")))
hacswiss2 <- as.vector(t(read.table("protestant.txt", sep=",")))

hacswiss <- cbind(hacswiss1, 1)
hacswiss_2 <- cbind(hacswiss2, 2)
hacswiss_new <- rbind(hacswiss, hacswiss_2)
hacswiss_sorted <- hacswiss_new[order(hacswiss_new[,1]),]
hac.factor <- as.factor(hacswiss_sorted[,2])
swiss_new <- cbind(swiss_copy, hac.factor)

scatterplot(Catholic ~ Examination | hac.factor, data = swiss, 
            smoother=NULL,reg.line=NULL,main="HAC")

# Catholic-predominant cities
rownames(swiss[hacswiss1,])
# Protestant-predominant cities
rownames(swiss[hacswiss2,])
