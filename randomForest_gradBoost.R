getwd()


library(tidyverse)
library(caret)
library(e1071)
library(MLmetrics)
library(randomForest)
library(ROCR)
library(pROC)
library(gbm)


train <- read.csv(file = 'data/train.csv')

train %>% str()
train <- train %>% map(as.factor) %>% as.data.frame()


index <- sample(x = 1:2, size = nrow(train), prob = c(0.7, 0.3), replace = TRUE)
test  <- train[index == 2,]
train <- train[index == 1,]

train$hand %>% table() %>% prop.table()*100
test$hand  %>% table() %>% prop.table()*100


fit_random_forest <- randomForest(x = train[,-11], y = train[,11],
                                  xtest = test[,-11], ytest = test[,11],
                                  ntree = 1000, mtry = 3,
                                  importance = TRUE,
                                  do.trace = 50,
                                  keep.forest = TRUE)

pred <- predict(object = fit_random_forest, newdata = test, type = 'response')
real <- test$hand

confusionMatrix(pred, reference = real, positive = '1')

#                      Class: 0 Class: 1  Class: 2  Class: 3 Class: 4 Class: 5 Class: 6  Class: 7  Class: 8  Class: 9
# Sensitivity            0.9562   0.4405 0.0025773 0.0060241 0.000000 0.000000 0.000000 0.0000000 0.0000000 0.0000000
# Specificity            0.5077   0.8432 1.0000000 1.0000000 1.000000 1.000000 1.000000 1.0000000 1.0000000 1.0000000


F1_Score(y_true = real, y_pred = pred, positive = '1')    # 0.5331337





# -------------------------------------------------------------------------------------------------------

fit_grad_boost <- gbm(formula = hand ~.,
                      data = train,
                      distribution = 'multinomial',
                      n.tree = 5000,
                      interaction.depth = 3,
                      shrinkage = 0.07,
                      n.minobsinnode = 10,
                      bag.fraction = 0.5,
                      cv.folds = 3,
                      n.cores = 1)


pred2 <- predict(object = fit_grad_boost, newdata = test, type = 'response')
real <- test$hand

confusionMatrix(pred2, reference = real, positive = '1')

F1_Score(y_true = real, y_pred = pred2, positive = '1')   






