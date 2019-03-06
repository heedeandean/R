# 피보나치 수열.

while(TRUE) {
  x = as.integer(readline(prompt = "Input the number: "))
  v = c(0, 1)
  
  if (x <= 0) break
  
  else if (x == 1) 
    print(v)
  
  else {
    for (i in 1:(x-1)) {
      v[length(v) + 1]  = v[length(v)] + v[length(v) - 1]
    }
    print(v)
  } 
}



