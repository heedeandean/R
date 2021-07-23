# TO DO! 목적변수() 예측

rm(list=ls())
library(tidymodels)

setwd('C:/Users/82102/OneDrive/바탕 화면/git/R/analysis/data/water/')

# =====================================
## 1. load data
data <- read.csv('data.csv', stringsAsFactors = F, strip.white = T)
glimpse(data) # 149 X 16
head(data)
# View(data)

# =====================================
## 2. NA 
colSums(is.na(data))
