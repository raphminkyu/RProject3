library(tidyverse)
library(plotly)
getwd()
train<- read.csv("data/train.rds")
test <- read.csv("data/test.csv")


set.seed(1234)
index <- sample(1:2,
                size = nrow(train),
                prob = c(0.7, 0.3),
                replace = TRUE)

trainSet <- train[index == 1, ]
testSet <- train[index == 2, ]


# KNN
library(class)
k <- trainSet %>% nrow() %>% 
  sqrt() %>% ceiling


fitKnn <- knn(trainSet[ , 1:10], 
              testSet[ , 1:10],
              cl = trainSet$hand,
              10,
              prob = TRUE)
?knn
fitKnn
ind_go<-trainSet$hand%in%4:6


head(trainSet)
?knn
trainSet


fitKnn <- knn(trainSet[ , 1:10], 
              test[ , 2:11],
              cl = trainSet$hand,
              k,
              prob = TRUE)

trainSet[ind_go , 1:10]
table(fitKnn)

table(trainSet$hand)
model <- fitKnn
modelTest <- testSet$hand
table(model, modelTest) 

library(caret)
# confusionMatrix(data = model, reference = modelTest) %>% glimpse()
confusionMatrix(data = model, reference = modelTest)
str(model)
model
modelTest
c$table %>% View()
?knn


library(MLmetrics)
F1_Score(y_pred = model, y_true = modelTest, positive = "best")
?F1_Score

# Doesn't work due to matrix not being 2x2
# 
# library(ROCR)

# 
# predObj <- prediction(predictions = as.numeric(model),
#                       labels = as.numeric(modelTest))
# 
# 
# perform <- performance(prediction.obj =predObj,
#                        measure = "tpr", 
#                        x.measure = "fpr" #false positieve rate
# )
# 
# plot(perform, main = "ROC curve")



# library(pROC)
# # auc(response = as.numeric(model),
# #     predictor = as.numeric(modelTest))
# multiclass.roc(response = as.numeric(model),
#                predictor = as.numeric(modelTest))

?kknn

library(kknn)
fitKnnW <- kknn(formula = handName ~.,
                train = trainSet,
                test = testSet,
                k = k,
                kernel = "triangular"
)
fitKnnW$fitted.values 
fitKnnW$fitted.values -> predW

confusionMatrix(predW, reference = modelTest, positive = "best")
F1_Score(y_pred = predW, y_true =  modelTest, positive = "best")



# one class classifaction (occ) #하나 찾는거 


