rm(list = ls())
# Given patterns
s1 = rbind(c(1, -1, 1, 1))
s2 = rbind(c(1, 1, 1, -1))
print(s1,s2)
# Transpose of patterens
k1 = t(s1)
k2 = t(s2)
# weight array declare
w = array(0,dim=c(4,4))
w1 = k1%*%s1 
w2 = k2%*%s2

# Zero out diagonal variables for 1st pattern
print(diag(w1) <- 0)

# Zero out diagonal variables for 1st pattern
print(diag(w2) <- 0)
print(w2)

# Final weight calculations
w = w1 + w2
print(w)
# Here we need to pass the vector( from config space) to test that it is basins or spurious
x = rbind(c(1, 1, 0, 1))

# Assign the value of x to y
y = x

cnt = 1
# In actually execution order of neuron is random but here already provided so I am folowing that only else I need to do it with random
order_r = c(1,4,3,2)
step = 0
change = c(1,1,1,1)
while(cnt > 0)
{
  cnt = 0
for(i in 1:4)
  {
  sum = 0
  
  print(order_r[i])
    y_in = y[order_r[i]]
    
    
    for(j in 1:4)
    {
      
    if(order_r[i] != j){
      
      sum = sum + y[j]*w[j,order_r[i]]
      
    }
    j = j+1
    }
    
    y_in = y_in + sum
    
    step = step + 1
    
    if(y_in > 0){
      
      if(y[order_r[i]] != 1){
        y[order_r[i]] = 1
        
        change[order_r[i]] = 0
         cnt = cnt + 1
      }
      else{
        print(paste0(i, " th neuron not changed change"))
      }
      
      print(y)
      
    }
    else if(y_in < 0){
      
      
      if(y[order_r[i]] != 0){
        y[order_r[i]] = 0
        
        change[order_r[i]] = 0
        cnt = cnt + 1
      }
      else{
        
      }
      y[order_r[i]] = 0
      
      print(y)
      
      print(paste0(i, " th neuron change"))
      
    }
    else
    {
      
      y[order_r[i]] = y[order_r[i]]
      
      print(y)
      
    }
    
    i = i+1
    
    print("=========Done with one neuron=====")
    
}
  if(cnt == 0)
    break
}
