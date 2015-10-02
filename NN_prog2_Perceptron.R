# Perceptron Algorithm Implementation
s <- array(c(1, 1, 1,-1,1,1,-1,1,1,-1,1,1),dim=c(4,3))
t <- array(c(1, -1, -1,-1),dim=c(4,1))
print(s)
print(t)
w = t(c(0,0,0))
b = 0
pattern_match <- 0
k<-1 # To Count Iteration
n<-0 #
step <- 0
alpha = 1
theta = 0.2
Y <- 0
print(-theta)
while (n < 4){
  

  for (i in 1:4)
  { 
    if(n >= 4)
    {
      break
    }
    step <- step + 1
    print(paste0("Inner loop iteration ", i))
    print(s[i,])
    print(paste0("above is ",i," Pattern"))
  y <- sum(s[i,]*w) + b
  print(paste0("Value of y in is ", y))
  
  if(y > theta ){
    print("i am in the loop")
    Y <- 1
    if(Y == t[i]){
      print("You have classified pattern correctly.....No need to update weight & Bias..!!!")
      n = n + 1;
      Y <- 0
    }
    else{
      print("Wrong Classification....Need to Upadte weights and Bias....")
      w = w + (alpha*t[i]*s[i,])
      b = b + (alpha*t[i])
      print("New Weights and Bias are below...")
      print(w)
      print(b)
      n<-0
    }
  }
  
  if(y < -theta){
    print("i am in the loop")
    Y <- -1
    if(Y == t[i]){
      print("You have classified pattern correctly.....No need to update weight & Bias..!!!")
      n = n +1
      Y <- 0
    }
      else{
        print("Wrong Classification....Need to Upadte weights and Bias....")
        w = w + (alpha*t[i]*s[i,])
        b = b + (alpha*t[i])
        print("New Weights and Bias are below...")
        n <- 0
        print(w)
        print(b)
      }
    
  }
  
  if(y >= -theta & y <= theta){
    Y <- 0
    if(Y != t[i]){
      print("You have wrong classification.....weights and Bias will change..!!!")
      w = w + (alpha*t[i]*s[i,])
      print("below is your new weight...")
      print(w)
      print("Below is your new Bias...")
      b = b + (alpha*t[i])
      print(b)
    }
  }
  
  }
  print(paste0("==============================Iteration ",k ," is done=============================="))
  print("Below is weight & Bias after above iteration")
  k = k + 1
print(w)
print(b)
print(paste0("Total",step," steps required to converge"))
#print(n)
print("***********************************You done with all after.......single step************************************")
}
