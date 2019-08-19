library(tidyverse)
library(plotly)

trainSet <- read.csv("data/train.csv")

str(trainSet$hand)

trainSet %>% glimpse() 

#factor since they are all categorical
trainSet[1:ncol(trainSet)] <- map(trainSet, as.factor)

# EDA


handNameV <- c("None", "One Pair", "Two Pairs", "Three of a Kind", "Straight", "Flush", "Full House", "Four of a Kind", "Straight Flush", "Royal Flush")

# trainSet$hand %>% factor()
trainSet$handName <- trainSet$hand
levels(trainSet$handName) <- handNameV

# write_rds(trainSet, "data/trainSetFactor.rds")
getwd()
# Frequency of different hand


trainSet %>% 
  group_by(handName) %>% 
  summarise(n = n()) %>% 
  plotly::plot_ly(labels = ~handName, values = ~n,type = "pie") %>% 
  plotly::layout(title = "Frequency of Poker Hands")

trainSet$handName %>% table()
trainSet$handName %>% table()  %>% prop.table() %>% round(5)

# Frequency of suits and ranks
trainSet %>% 
  purrr::keep(str_detect(colnames(trainSet), "S.")) %>% 
  c(recursive = TRUE, use.names= FALSE) %>% table() %>% prop.table() %>% round(3) %>% as.data.frame() %>% 
  plotly::plot_ly(labels = ~`.`, values = ~Freq,type = "pie") %>% 
  plotly::layout(title = "Frequency of Suits")

trainSet %>% 
  purrr::keep(str_detect(colnames(trainSet), "C.")) %>% 
  c(recursive = TRUE, use.names= FALSE) %>% table() %>% prop.table() %>% round(3) %>% as.data.frame() %>% 
  plotly::plot_ly(labels = ~`.`, values = ~Freq,type = "pie") %>% 
  plotly::layout(title = "Frequency of Ranks")

# Does the Poker Hand Overlap 
trainSet %>% 
  filter(handName == "Straight Flush") %>% count()

trainSet %>% 
  filter(handName == "Straight") %>% count()


trainSet %>% 
  filter(handName =="Straight") %>%
  purrr::keep(str_detect(colnames(trainSet), "S.")) %>% 
  t() %>% 
  as.data.frame() %>% 
  unique() %>%
  sapply(length) %>% 
  unique() %>% 
  {ifelse(.==1 , T, F)} %>% 
  all()

  
trainSet %>% 
  filter(handName =="Straight Flush") %>%
  purrr::keep(str_detect(colnames(trainSet), "S.")) %>% 
  t() %>% 
  as.data.frame() %>% 
  unique() %>%
  sapply(length) %>% 
  {ifelse(.==1 , T, F)} %>% 
  all()
  
# Thus Straight Flush not included in Straight



