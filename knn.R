library(tidyverse)
library(plotly)

trainSet <- read.csv("data/train.csv")

str(trainSet$hand)

trainSet %>% glimpse() 

#factor since they are all categorical
trainSet[1:ncol(trainSet)] <- map(trainSet, as.factor)

# EDA
freq <- trainSet$hand %>% table() 
freqP <- freq %>% prop.table() %>% round(5)

hand <- c("None", "One Pair", "Two Pairs", "Three of a Kind", "Straight", "Flush", "Full House", "Four of a Kind", "Straight Flush", "Royal Flush")

# Frequency of different hand
trainSet %>% 
  ggplot(mapping = aes(trainSet$hand)) +
  scale_x_discrete(labels = hand)+
  geom_bar()

# Frequency of suits and numbers
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


