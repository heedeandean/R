exam <- read.csv("C:/Users/user/Desktop/공부예제/R예제/Data/csv_exam.csv")
head(exam, 10)
tail(exam, 10)
View(exam)
dim(exam)
str(exam)
summary(exam)

# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("Rcpp")
library(dplyr)
library(ggplot2)

df_raw <- data.frame(var1 = c(1, 2, 1),
                     var2 = c(2, 3, 2))
df_new <- df_raw
df_new
df_new <- rename(df_new, v2 = var2)

mpg <- as.data.frame(ggplot2::mpg)
# mpg <- rename(mpg, city = cty)
# mpg <- rename(mpg, highway = hwy)

head(mpg)


df <- data.frame(var1 = c(4, 3, 8),
                 var2 = c(2, 6, 1))
df$var_sum <- df$var1 + df$var2
df$var_mean <- df$var_sum / 2

mpg$total <- (mpg$cty + mpg$hwy) / 2
mean(mpg$total)
summary(mpg$total)
hist(mpg$total)
mpg$test <- ifelse(mpg$total >= 20, "pass", "fail")
table(mpg$test)
qplot(mpg$test)
mpg$grade <- ifelse(mpg$total >= 30, "A", 
                    ifelse(mpg$total >= 20, "B", "C"))
qplot(mpg$grade)

head(mpg)
