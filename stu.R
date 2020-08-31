install.packages("foreign")
library(foreign)
library(readxl)

raw_welfare <- read.spss(file = "C:/Users/user/Desktop/공부예제/R예제/Data/Koweps_hpc10_2015_beta1.sav",
                         to.data.frame = T)
welfare <- raw_welfare

head(welfare)
View(welfare)
dim(welfare)
