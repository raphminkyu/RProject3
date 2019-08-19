library(tidyverse)
library(plotly)
getwd()
trainSet<- read_rds("data/trainSetFactor.rds")
testSet <- read.csv("data/test.csv")
trainSet2 <- read.csv("data/train.csv")

trainSet <- trainSet2[, 2:12]



knn_predict <-knn(trainSet[], testSet, trainCLASS, k = 10, l = 0, prob = FALSE, use.all = TRUE)

?scale

