library(tidyverse)
library(plotly)
getwd()
train<- read_rds("data/trainSetFactor.rds")


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


fitKnn <- knn(trainSet[ , 1:ncol(train)-1], 
              testSet[ , 1:ncol(train)-1],
              cl = trainSet$handName,
              k,
              prob = TRUE)

?confusionMatrix()
model <- fitKnn
modelTest <- testSet$handName
table(model, modelTest) %>% View()

library(caret)
# confusionMatrix(data = model, reference = modelTest) %>% glimpse()
confusionMatrix(data = model, reference = modelTest)$table %>% View()


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

