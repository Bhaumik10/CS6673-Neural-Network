# Perceptron Algorithm Implementation
s <- array(c(1,-1, 1,1,1,1,1,-1,1,-1,1,-1,1,-1,-1,1),dim=c(4,4))
print(nrow(s))
print(s)
t <- array(c(1, 1, -1,-1),dim=c(4,1))
print(t)
w = t(c(0,0,0,0))
w_memory = t(c(0,0,0,0))
b = 0
b_memory = 0
status = 1
j = 1
#alpha = 0.15

while(TRUE)
{
  tolerance = 0.001
  
  #alpha = 0.15/j
  for (i in 1:nrow(s))
  { 
    y <- sum(s[i,]*w) + b
    alpha = 0.15/((j-1)*4 + i)
    print(y)
    wnew = w - (2*alpha*(y - t[i])*s[i,])
    bnew = b - (2*alpha*(y - t[i]))
    w_memory = w_memory + wnew
    b_memory = b_memory + bnew
    
    if (i == 2 || i == 4)
    {
      w_new = w_memory/2
      b_new = b_memory/2
      delta_w = sqrt(sum((w_new - w) ^ 2))
      delta_b = b_new - b
      w = w_new
      b= b_new
      w_memory = 0
      b_memory = 0
      if(delta_w < tolerance)
      {
        status = 0
        break
      }
    }
    
    print("New Weights and Bias are below...")
   
    print(w)
    print(paste0("*****delta_w == ",delta_w))
    print(b)
    print(paste0("*****delta_b == ",delta_b))
    print("================================")
    

  }  
  print(paste("value of alpha is :  :",alpha))
  print(paste0("iteration no is >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>", j))
  j = j + 1;     
  print(w)
  print(delta_w)
  print(b)
  print(delta_b)
  if (status == 0 )
    break
  
}
