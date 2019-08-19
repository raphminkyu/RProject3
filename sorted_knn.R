library(tidyverse)
library(class)
library(MLmetrics)
library(caret)

train <- read.csv(file = "data/train.csv")
# test <- read.csv(file = "data/test.csv")

train <- train %>% select(starts_with("C"), "hand")
# test <- test %>% select(starts_with("C"))

train <- train %>%
  mutate(flush = ifelse(hand == 5, 1, 0))
# test <- test %>%
#   mutate(flush = ifelse(hand == 5, 1, 0))


hand <- train[  ,c("flush", "hand")]

train <- t(apply(train[1:5], 1, sort, TRUE))
colnames(train) <- c("C1", "C2", "C3", "C4", "C5")
sorted_train <- cbind(train, hand) %>% as.data.frame()

sorted_train <- sorted_train %>% map_df(as.factor)

# saveRDS(sorted_train, "data/sorted_train.rds")
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

fitProb <- knn(train = train[, 1:ncol(train)-1],
           test  = test[, 1:ncol(test)-1],
           cl    = train$hand,
           k     = 1,
           prob  = FALSE)



pred <- fit
real <- test$hand



confusionMatrix(data = pred, reference = real)  # Accuracy : 0.9505


F1_Score(y_true = real, y_pred = pred)   # 0.9879612

fitProb %>% length()

testCheck <- cbind(test, fitProb)

testCheck[1 , ]

result <- data.frame()
for(i in 1:nrow(testCheck)){
  if(testCheck[i, 7] != testCheck[i , 8]){
    result <- rbind(result, testCheck[i , ])
  }
  
}

result %>% 
  filter(hand == 5)


testCheck %>% 
  filter(hand == 3) %>% View()
