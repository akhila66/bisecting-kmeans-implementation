

library(tidyverse)
library(readr)
# ----------
data <- read.csv("/Users/akhilapodupuganti/Desktop/SEM2/DM/HW4/dow_jones_index.data",header=TRUE,sep = ",")

colnames(data)

data = data[-c(1,2,3)]

colnames(data)

data %>% mutate_all(is.na) %>% summarise_all(mean) # checking missingness

data$open <- as.numeric(gsub('\\$|,', '', data$open)) # removing $
data$high <- as.numeric(gsub('\\$|,', '', data$high)) # removing $
data$low <- as.numeric(gsub('\\$|,', '', data$low)) # removing $
data$close <- as.numeric(gsub('\\$|,', '', data$close)) # removing $
data$next_weeks_open <- as.numeric(gsub('\\$|,', '', data$next_weeks_open)) # removing $
data$next_weeks_close <- as.numeric(gsub('\\$|,', '', data$next_weeks_close)) # removing $

# replaceing missing ness with mean
for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}

data %>% mutate_all(is.na) %>% summarise_all(mean) # checking missingness




boxplot(data)


totss = c()
kclusters = c(2:7)
for(k in kclusters)
{
print("--------------------")
print(paste('for ', k, 'clusters'))

clusters <- kmeans(data, k)

print('the sum of squared errors for each cluster ::')
print(paste(clusters$withinss))

print(paste('total sum of squared errors :: ',clusters$tot.withinss))
totss[k-1] <- clusters$tot.withinss # soring for future ploting

print('custer mean')
print(t(clusters$centers))

print('custer IDs')
print(clusters$cluster)

print('the IDs of all the instances belonging to that cluster')
for(a in c(1:k)){
  print(paste('for', k, '-means clustering --','for',a,'cluster'))
  IDs <- (which(clusters$cluster==a))
  temp = data[IDs,]
  print(IDs)
}
}


plot(kclusters,totss, type='o')
print(totss)

sdata <- scale(data)

# --------------------------------- bisecting with 3 trails and for largest SSE

Biclusters = c() # initilizing vector for  clusters
BiSSE = c(1) # vector for SSE and setting 1 as a sse for whole data
Biclusters[length(Biclusters)+1] <- list(as.numeric(row.names(data))) # taking whole data as one cluster
#-999999999
choosek = 5 # the k value choosen from above 3.3
while(length(Biclusters) < choosek ) # looping until we get 5 clusters
{
  MaxSSEId <- which(BiSSE %in% max(BiSSE)) # getting the max sse id 
  IDs <- unlist(Biclusters[MaxSSEId]) #getting max sse ids of the cluster
  MaxSSECluster <- data[IDs,] # getting data of max Sse using ids
  print('enterned while')
  Biclusters <- Biclusters[-MaxSSEId] # removing that choosen cluster
  BiSSE <- BiSSE[-MaxSSEId] # removing respective choosen cluster sse from list
  print('------------------------')

  lessSSE <- 99999999999999999999999
  lessSSEbikm <- c() 
  for(i in c(1,2,3)) # 3 trails
  {
    # bisecting cluster for 3 trails 
    print(paste('entered bisecting',i))
    bikm <- kmeans(MaxSSECluster,2) # bisecting
    if(bikm$totss < lessSSE) # checking if it is less sse 
    {
      lessSSEbikm <- bikm # if so choosing
      lessSSE <- bikm$totss 
    }
    
  }
  CustID1 <- IDs[which(as.vector(lessSSEbikm$cluster)==1)] # id's of the choosen trail cluster 1
  CustID2 <- IDs[which(as.vector(lessSSEbikm$cluster)==2)]# id's of the choosen trail cluster 2
  Biclusters[length(Biclusters)+1] <- list(CustID1) # saving them into out list of clusters
  Biclusters[length(Biclusters)+1] <- list(CustID2)
  BiSSE <- append(BiSSE, lessSSEbikm$withinss)

} # repeat the process with the largest sse cluster untill we get 5 clusters

#total sum of sse
print(sum(BiSSE))
print(totss)
# cluster means
clust.mean = function(i, dat, clusters) {
  print(i)
  ind = unlist(clusters[i])
  colMeans(dat[ind,])
}
sapply(1:5, clust.mean, data, Biclusters)



# ----------------------------- 3.5
library("cluster")
library("dendextend")

k_H_Cluster <- hclust(dist(as.matrix((data)),method = "euclidean"),method = "single")
plot(k_H_Cluster)
kcut <-cutree(k_H_Cluster, k = 5) # choosing k variable
for(a in c(1:5)){
  IDs <- (which(kcut==a))
  print(IDs)
}

out <-css(dist(as.matrix(data),method = "euclidean"),kcut)
print(out$wss) #individual sse
print(out$totwss) # total sse

library("dendextend")
den <- as.dendrogram(k_H_Cluster)
den <- color_branches(den, h = 5)
plot(den)
# -------------------------

# cleaning
rm(list = ls(all.names = TRUE))
gc()
