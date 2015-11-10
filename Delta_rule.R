# Perceptron Algorithm Implementation
s <- array(c(1,-1, 1,1,1,1,1,-1,1,-1,1,-1,1,-1,-1,1),dim=c(4,4))
print(nrow(s))
print(s)
t <- array(c(1, 1, -1,-1),dim=c(4,1))
print(t)
w = t(c(0,0,0,0))
b = 0
status = 1
j = 1
#alpha = 0.15

while(status > 0 )
{
  tolerance = 0.001
  
  #alpha = 0.15/j
  for (i in 1:nrow(s))
  { 
    y <- sum(s[i,]*w) + b
    alpha = 0.15/((j-1)*4 + i)
        wnew = w - (2*alpha*(y - t[i])*s[i,])
        bnew = b - (2*alpha*(y - t[i]))
        delta_w = sqrt(sum((wnew - w) ^ 2))
        delta_b = bnew - b
        
        print("New Weights and Bias are below...")
        w = wnew
        b = bnew
        print(w)
        print(paste0("*****delta_w == ",delta_w))
        print(b)
        print(paste0("*****delta_b == ",delta_b))
        print("================================")
        if(delta_w < tolerance)
        {
          status = 0
          break
        }
        
  }  
  print(paste("value of alpha is :  :",alpha))
  print(paste0("iteration no is >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>", j))
  j = j + 1;     
  print(paste0("This is delta_b : ",delta_b))
  print(paste0("This is delta_w : ",delta_w))
  if(status == 0){
    break
  }
  
   
}
    