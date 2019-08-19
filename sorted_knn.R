library(tidyverse)
library(class)

train <- read.csv(file = "train.csv")

train <- train %>% select(starts_with("C"), "hand")
str(train)

sorted_train <- data.frame(C1 = rep(0, nrow(train)),
                           C2 = rep(0, nrow(train)), 
                           C3 = rep(0, nrow(train)), 
                           C4 = rep(0, nrow(train)), 
                           C5 = rep(0, nrow(train)),
                           hand = train$hand)


for(i in 1:nrow(train)){
  
  vector <- c()
  vector <- train[i,] %>% 
    map_int(.f = function(x){vector <- append(x, vector); return(vector)}) %>% 
    sort(decreasing = TRUE) %>% 
    '['(1:5)
  
  sorted_train$C1[i] <- vector[1]
  sorted_train$C2[i] <- vector[2]
  sorted_train$C3[i] <- vector[3]
  sorted_train$C4[i] <- vector[4]
  sorted_train$C5[i] <- vector[5]
  
}


sorted_train <- sorted_train %>% map_df(as.factor)





# Model fitting ---------------------------------------------------------------------------


set.seed(1234)
index <- sample(1:2, size = nrow(sorted_train), prob = c(0.7, 0.3), replace = TRUE)


test  <- sorted_train[index == 2,]
train <- sorted_train[index == 1,]







fit <- knn(train = train[, 1:ncol(train)-1],
           test  = test[, 1:ncol(test)-1],
           cl    = train$hand,
           k     = 1,
           prob  = TRUE)


pred <- fit
real <- test$hand

library(caret)
confusionMatrix(data = pred, reference = real)  # Accuracy : 0.9505

library(MLmetrics)
F1_Score(y_true = real, y_pred = pred)   # 0.9879612



