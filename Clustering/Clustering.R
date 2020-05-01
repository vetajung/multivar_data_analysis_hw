# Multivate Data Analysis Assignment #2
# 2015170378 Jung Eun-young

install.packages("ISLR")
install.packages("clValid")
install.packages("plotrix")

library(ISLR)
library(clValid)
library(plotrix)

data(College)
View(College)

c_Private <- College[,1]
cX <- College[,-1]
cX_scaled <- as.matrix(scale(cX, center = TRUE, scale = TRUE))

# -------------[K-means Clustering]-------------

# [Q1-1]
## internal validation

start_time <- Sys.time()
cX_clValid <- clValid(cX_scaled, 2:10, clMethods="kmeans",
                                 validation=c("internal","stability"))
end_time <- Sys.time()

## view results
end_time - start_time
summary(cX_clValid)

#[Q1-2]
#n은 반복수, k는 군집수
Kmeans_Cluster_Info <- function(k,n){
  for(i in 1:n){
    cX_kmc <- kmeans(cX_scaled,k)
    print(paste("n=",i))
    print(cX_kmc$centers)
    print(cX_kmc$size)
  }
}
# Perform K-Means Clustering with the best K determined by Silhouette
Kmeans_Cluster_Info(3,10)

#[Q1-3]
Kmeans_Cluster_Info(10,10)

#[Q1-4]
cX_kmc <- kmeans(cX_scaled,3)
# Compare the cluster info. and class labels
real_private <- c_Private
kmc_cluster <- cX_kmc$cluster
table(real_private,kmc_cluster)

# Compare each cluster for KMC
cluster_kmc <- data.frame(cX_scaled, clusterID = as.factor(cX_kmc$cluster))
kmc_summary <- data.frame()

for (i in 1:(ncol(cluster_kmc)-1)){
  kmc_summary = rbind(kmc_summary, 
                      tapply(cluster_kmc[,i], cluster_kmc$clusterID, mean))
}

colnames(kmc_summary) <- paste("cluster", c(1:3))
rownames(kmc_summary) <- colnames(cX)

par(mfrow = c(2,2))
for (i in 1:3){
  plot_title <- paste("Radar Chart for Cluster", i, sep=" ")
  radial.plot(kmc_summary[,i], labels = rownames(kmc_summary), 
              radial.lim=c(-2,2), rp.type = "p", main = plot_title, 
              line.col = "red", lwd = 2.5, show.grid.labels=1)
}
dev.off()

#[Q1-5]
#Cluster_tTest(method,ClusterIndex(i),ClusterIndex(j))
Cluster_tTest <- function(m,n1,n2){
  
  if(m=="kmc"){
    cluster1 <- cX[cX_kmc$cluster == n1,]
    cluster2 <- cX[cX_kmc$cluster == n2,]
    t_result <- data.frame()
  }else if(m=="hc"){
    cluster1 <- cX[cluster_hc$cluster == n1,]
    cluster2 <- cX[cluster_hc$cluster == n2,]
    t_result <- data.frame()
  }
  
  for (i in 1:ncol(cX)) {
    
    t_result[i,1] <- t.test(cluster1[,i], cluster2[,i], 
                                alternative = "two.sided")$p.value
    
    t_result[i,2] <- t.test(cluster1[,i], cluster2[,i], 
                                alternative = "greater")$p.value
    
    t_result[i,3] <- t.test(cluster1[,i], cluster2[,i], 
                                alternative = "less")$p.value
  }
  print(t_result)
}

#Cluster_tTest(method,ClusterIndex(i),ClusterIndex(j))
# Cluster 1 vs. Cluster 2
Cluster_tTest("kmc",1,2)

# Cluster 1 vs. Cluster 3
Cluster_tTest("kmc",1,3)

#Cluster 2 vs. Cluster 3
Cluster_tTest("kmc",2,3)


#[Q1-6]
install.packages("fpc")
library(fpc)
plotcluster(cX_scaled, cX_kmc$cluster, color=TRUE, shade=TRUE)

#-------------[Hierarchical Clustering]-------------

#[Q2-1]

## internal validation
start_time <- Sys.time()
cX_clValid <- clValid(cX_scaled, 2:10, clMethods="hierarchical",
                        validation=c("internal","stability"))
end_time <- Sys.time()

## view results
end_time - start_time
summary(cX_clValid)

#[Q2-2]
## Compute the similarity using the spearman coefficient

cor_Mat <- cor(t(cX_scaled), method = "spearman")
dist_cX <- as.dist(1-cor_Mat)

## Perform hierarchical clustering
hcMethod <- c("complete","average","single","mcquitty","median","centroid","ward.D","ward.D2")
for (item in hcMethod){
  hr <- hclust((dist_cX), method = item, members=NULL)
  plot(hr)
}


#[Q2-3]
# Find the clusters
hr <- hclust((dist_cX), method = "complete", members=NULL)
mycl <- cutree(hr, k=10)
mycl

plot(hr)
rect.hclust(hr, k=10, border="red")


# Compare each cluster for HC
cluster_hc <- data.frame(cX_scaled, clusterID = as.factor(mycl))
hc_summary <- data.frame()

for (i in 1:(ncol(c_hc)-1)){
  hc_summary = rbind(hc_summary, 
                     tapply(cluster_hc[,i], cluster_hc$clusterID, mean))
}

colnames(hc_summary) <- paste("cluster", c(1:10))
rownames(hc_summary) <- colnames(cX)
hc_summary

# Radar chart
par(mfrow = c(3,4))
for (i in 1:10){
  plot_title <- paste("Radar Chart for Cluster", i, sep=" ")
  radial.plot(hc_summary[,i], labels = rownames(hc_summary), 
              radial.lim=c(-2,2), rp.type = "p", main = plot_title, 
              line.col = "red", lwd = 3, show.grid.labels=1)
}
dev.off()

# Compare the cluster 
# Hc_tTest(method,ClusterIndex(i),ClusterIndex(j))
# 차이가 가장 비슷한 집단 Cluster 6  vs. Cluster 7
Cluster_tTest("hc", 6, 7)

# 차이가 가장 극명한 집단 Cluster 3 vs. Cluster 8
Cluster_tTest("hc", 3, 8)

#[Q2-4]
library(RColorBrewer)
coul = colorRampPalette(brewer.pal(8, "PiYG"))(25)
heatmap(data.matrix(cluster_hc),Rowv=NA, Colv=NA, col=coul, scale="column", margin=c(5,10))


#[DBSCAN]
ploan <- read.csv("Personal Loan.csv")
ploan_x <- ploan[,-c(1,5,10)]
ploan_x_scaled <- scale(ploan_x, center = TRUE, scale = TRUE)

install.packages("factoextra")
install.packages("dbscan")

library(factoextra)
library(dbscan)
library(fpc)

#[Q3-1] 
# DBSCAN & Visualization
nMat <- matrix(c(1:100),nrow=25,ncol=4)
colnames(nMat)<- c("eps", "minPts","군집 수","Noise 수")

eps <- c(1.5,2,2.5,3,3.5)
minPts <- c(10,11,12,13,14)
for (i in 1:5){
  for(j in 1:5){
    nMat[(5*i-5)+j,1] <- eps[i]
    nMat[(5*i-5)+j,2] <- minPts[j]
    nMat[(5*i-5)+j,3] <- length(unique(dbscan(ploan_x_scaled, eps = eps[i], MinPts = minPts[j])$cluster))-1
    nMat[(5*i-5)+j,4] <- length(which((dbscan(ploan_x_scaled, eps = eps[i], MinPts = minPts[j])$cluster)==0))
  }
}
db_table<-as.data.frame(nMat)

#[Q3-2] //서다인 학우의 코드를 참고하였습니다.
DBSCAN_multishapes <- dbscan(ploan_x_scaled, eps = 3, MinPts = 10)

cluster_kmc <- data.frame(ploan_x_scaled, clusterID = as.factor(DBSCAN_multishapes$cluster))
kmc_summary <- data.frame()

for (i in 1:(ncol(cluster_kmc)-1)){
  kmc_summary = rbind(kmc_summary, 
                      tapply(cluster_kmc[,i], cluster_kmc$clusterID, mean))
}
kmc_summary[,-1]

colnames(kmc_summary) <- paste("cluster", c(1:5))
rownames(kmc_summary) <- colnames(ploan_x)

par(mfrow = c(2,3))
for (i in 1:5){
  plot_title <- paste("Radar Chart for Cluster", i, sep=" ")
  radial.plot(kmc_summary[,i], labels = rownames(kmc_summary), 
              radial.lim=c(-2,2), rp.type = "p", main = plot_title, 
              line.col = "red", lwd = 2.5, show.grid.labels=1)
}
dev.off()


#[Q3-3] 
fviz_cluster(DBSCAN_multishapes, ploan_x_scaled, ellipse = FALSE, geom = "point",
             show.clust.cent = FALSE)
