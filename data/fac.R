while(TRUE) {
  x <- as.integer(readline(prompt = "Input the number: "))
  f <- 1
  
  if (x <= 0) break
  else {
    for (i in 1:x)
      f <- f * i
    
    print(paste("The factorial of", x, "is", f))
  } 
}
