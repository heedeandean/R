foo <- data.frame(actual=c(1,0,5,0,4,-2,2,3,0,5), pred=c(2,1,0,0,-2,-5,-1,3,2,3))
foo

# 실제값>예측값인 경우 (1-예측값/실제값), 
# 실제값<예측값인 경우 (1- 실제값/예측값)으로 하고 이것들의 평균을 구하는 함수.
test <- function(actual, pred) {
  result = ifelse(actual > pred, 1-pred/actual, 1-actual/pred)
  
  result[is.nan(result)] = 0
  result[is.infinite(result)] = 0
  
  return(mean(result, na.rm = T))
}

test(foo$actual, foo$pred)

0/0 # NaN
1/0 # Inf
NA
