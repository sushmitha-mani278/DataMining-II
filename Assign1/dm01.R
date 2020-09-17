#DATA MINING-2 ASSIGNMENT 2

#install.packages("useful")
library(useful)
#install.packages("factoextra")
library(dplyr)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

setwd("C:/Users/97150/Desktop/SUMMER")

df<- read.csv("C:/Users/97150/Desktop/SUMMER/udemy_courses.csv")
View(df)

#K-means
options(digits = 2)
expand.grid(c(-3,3),c(-3,3),c(-3,3))


#data(df,package = "ClustOfVar")

dim(df)
head(df)
tail(df)
glimpse(df)

options(digits = 1)

df <- df[order(df$num_subscribers),]
v_keep <- c("num_reviews","num_lectures","num_subscribers"
)
View(v_keep)
M <- df[,v_keep]
View(M)
scale_M <- scale(M)





#Two cluster K-means
kmeans_2<-kmeans(
  x = scale_M,
  centers = 2
)
kmeans_2

#Three cluster K-means
kmeans_3<-kmeans(
  x = scale_M,
  centers = 3
)
kmeans_3

# #Four cluster K-means 
# kmeans_4<-kmeans(
#   x = scale_M,
#   centers=4
# )
# kmeans_4


# kmeans_10<-kmeans(
#   x= scale_M,
#   centers=10
# )
# kmeans_10

plot_3cluster<-useful::plot.kmeans(
  x = kmeans_3,
  data = scale_M  
)
plot_3cluster


useful::plot.kmeans(
  x = kmeans_2,
  data = scale_M  
)

# 
# useful::plot.kmeans(
#   x = kmeans_4,
#   data = scale_M  
# )

#To find the optimal Kclusters
#kmeans-2
set.seed(823)
factoextra::fviz_nbclust(
  x = scale_M,
  FUNcluster = kmeans,
  method = "wss"
)

#clara - 3
set.seed(823)
factoextra::fviz_nbclust(
  x = scale_M,
  FUNcluster = clara,
  method = "wss"
)

# set.seed(823)
# factoextra::fviz_nbclust(
#   x = scale_M,
#   FUNcluster = kmeans,
#   method = "silhouette"
# )


#r-functions for kmeans 

clara_M <- cluster::clara(
  x = scale_M,
  k = 2
)
plot(clara_M)
print(clara_M)

# 
# #Fuzzy Analysis Clustering
# fanny_M <- cluster::fanny(
#   x = scale_M,
#   k = 2
# )
# 
# plot(fanny_M)
# print(fanny_M)
# 
# #Partitioning Around Medoids
# pam_M <- cluster::pam(M,k = 2)
# plot(pam_M)
# print(pam_M)






#Hierarchical unsupervised Clustering


#distance matrix-dataset
dist_M <- dist(scale_M[1:100,])
dist_M
heatmap(
  x = as.matrix(dist_M),
  col = viridis::viridis(256)
)


#hierarchical clustering dendoogram
hclust_M <- hclust(dist_M)
plot(hclust_M)

#principal component analysis:
cutree_M <- cutree(
  tree = hclust_M,
  k = 4
)
cutree_M
# 
# prcomp_M <- data.frame(
#   prcomp(
#     x = scale_M,
#     center = FALSE,
#     scale. = FALSE
#   )$x[,1:2],
#   Name = rownames(df),
#   Rank = scale_M$num_reviews,
#   Points = scale_M$num_subscribers,
#   Cluster = as.character(cutree_M),
#   stringsAsFactors = FALSE
# )
# 
# require(ggplot2)
# ## Loading required package: ggplot2
# ggplot(prcomp_M) +
#   aes(x = PC1,y = PC2,size = Points,color = Cluster,fill = Cluster,label = Name,group = Cluster) +
#   geom_point() +
#   ggrepel::geom_text_repel(color = "black",size = 3) +
#   ggtitle("Scatter plot of decathlon principal components","Color corresponds to k-means cluster") +
#   theme_bw() +
#   theme(legend.position = "none")



v_dist <- c(
"manhattan","euclidean"
  #,"binary"
)
list_dist <- lapply(
  X = v_dist,
  FUN = function(distance_method) dist(
    x = scale_M[1:100,],
    method = distance_method
  )
)
names(list_dist) <- v_dist
v_hclust <- c(
  "ward.D","single"
  #,"median","centroid"
)


#Selection of clustering:

list_hclust <- list()
for(j in v_dist) for(k in v_hclust) list_hclust[[j]][[k]] <- hclust(
  d = list_dist[[j]],
  method = k
)
par(
  mfrow = c(length(v_dist),length(v_hclust)),
  mar = c(0,0,0,0),
  mai = c(0,0,0,0),
  oma = c(0,0,0,0)
)
for(j in v_dist) for(k in v_hclust) plot(
  x = list_hclust[[j]][[k]],
  labels = FALSE,
  axes = FALSE,
  main = paste("\n",j,"\n",k)
)


plot(
  x = list_hclust[["manhattan"]][["ward.D"]],
  main = "manhattan Ward's D",
  sub = ""
)

plot(
  x = list_hclust[["euclidean"]][["ward.D"]],
  main = "euclidean ward.D",
  sub = ""
)


d <- dist(scale_M, method = "manhattan")
hc5 <- hclust(d, method = "ward.D")
cutree_M <- cutree(hc5, k = 4)
factoextra::fviz_cluster(list(data = scale_M, cluster = cutree_M))




v <- prcomp(scale_M)$x[,1]
scale_M <- scale_M[order(v),]


#Agnes- agglomerative clustering 
agnes_M <- cluster::agnes(scale_M[1:100,])


as.hclust(x = agnes_M)
print(agnes_M)
as.dendrogram(object = agnes_M)
cutree(tree = agnes_M, k = 4)


#diana - divisive analysis clustering
diana_M <- cluster::diana(scale_M[1:100,], metric = "manhattan")
plot(diana_M)
as.hclust(x = diana_M)
print(diana_M)
as.dendrogram(object = diana_M)
cutree(tree = diana_M, k = 4)
