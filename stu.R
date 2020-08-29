df <- data.frame(sex = c("M", "F", NA, "M", "F"),
                 score = c(5, 4, 3, 4, NA))
df
table(is.na(df))
table(is.na(df$sex))
mean(df$score)

df_nomiss <- df %>% filter(!is.na(score) & !is.na(sex))
df_nomiss2 <- na.omit(df)
df_nomiss2

mean(df$score, na.rm = T)
exam[c(3, 8, 15), "math"] <- NA
exam %>% summarise(mean_math = mean(math, na.rm = T))
mean(exam$math, na.rm = T)
exam$math <- ifelse(is.na(exam$math), 55, exam$math)
table(is.na(exam$math))

mpg[c(65, 124, 131, 153, 212), "hwy"] <- NA

table(is.na(mpg$drv))
table(is.na(mpg$hwy))

mpg %>% 
  filter(!is.na(hwy)) %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy))

outlier <- data.frame(sex = c(1, 2, 1, 3, 2, 1),
                      score = c(5, 4, 3, 4, 2, 6))
table(outlier$sex)
outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex)
outlier$score <- ifelse(outlier$score > 5, NA, outlier$score)
outlier %>% 
  filter(!is.na(sex) & !is.na(score)) %>% 
  group_by(sex) %>% 
  summarise(mean_score = mean(score))

mpg <- as.data.frame(ggplot2::mpg)
boxplot(mpg$hwy)$stats
