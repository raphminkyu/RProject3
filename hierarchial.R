install.packages("cluster")
install.packages("fpc")
library(cluster)
library(dplyr)
library(fpc)


set.seed(1234)

train<- readRDS("data/trainSetFactor.rds")
test <- read.csv("data/test.csv")

index <- sample(1:2,
                size = nrow(train),
                prob = c(0.7, 0.3),
                replace = TRUE)

trainSet <- train[index == 1, ]
testSet <- train[index == 2, ]

gower <- daisy(trainSet[c(1:1000), -12], metric = c("gower"))

class(gower)



divisive.clust <- diana(as.matrix(gower), 
                        diss = TRUE, keep.diss = TRUE)
plot(divisive.clust, main = "Divisive")


cstats.table <- function(dist, tree, k) {
  clust.assess <- c("cluster.number","n","within.cluster.ss","average.within","average.between",
                    "wb.ratio","dunn2","avg.silwidth")
  clust.size <- c("cluster.size")
  stats.names <- c()
  row.clust <- c()
  output.stats <- matrix(ncol = k, nrow = length(clust.assess))
  cluster.sizes <- matrix(ncol = k, nrow = k)
  for(i in c(1:k)){
    row.clust[i] <- paste("Cluster-", i, " size")
  }
  for(i in c(2:k)){
    stats.names[i] <- paste("Test", i-1)
    
    for(j in seq_along(clust.assess)){
      output.stats[j, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])[j]
      
    }
    
    for(d in 1:k) {
      cluster.sizes[d, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.size])[d]
      dim(cluster.sizes[d, i]) <- c(length(cluster.sizes[i]), 1)
      cluster.sizes[d, i]
      
    }
  }
  output.stats.df <- data.frame(output.stats)
  cluster.sizes <- data.frame(cluster.sizes)
  cluster.sizes[is.na(cluster.sizes)] <- 0
  rows.all <- c(clust.assess, row.clust)
  # rownames(output.stats.df) <- clust.assess
  output <- rbind(output.stats.df, cluster.sizes)[ ,-1]
  colnames(output) <- stats.names[2:k]
  rownames(output) <- rows.all
  is.num <- sapply(output, is.numeric)
  output[is.num] <- lapply(output[is.num], round, 2)
  output
}

stats.df.divisive <- cstats.table(gower, divisive.clust, 9)
stats.df.divisive
