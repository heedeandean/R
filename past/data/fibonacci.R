# 피보나치 수열. 0 1 1 2 3 5 8 

# 1안.

while(TRUE) {
  x <- as.integer(readline(prompt = "Input the number: "))
  v <- c(0, 1)
  
  if (x <= 0) break
  
  else if (x == 1) 
    print(v)
  
  else {
    for (i in 1:(x-1)) {
      v[length(v) + 1]  <- v[length(v)] + v[length(v) - 1]
    }
    print(v)
  } 
}

# 2안.

while(TRUE) {
  x <- as.integer(readline(prompt = "Input the number: "))
  if (x <= 0) break

  p2 <- 0
  p1 <- 1
  result <- paste(p2, p1)
  
  while(x > 1) {
    p <- p2 + p1
    p2 <- p1
    p1 <- p
    
    result <- paste(result, p)
    
    x <- x - 1
  }
  print(result)
}
