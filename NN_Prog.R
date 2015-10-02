
# image as Input
x1<- c(-1, 1, 1, 1, -1, 1, -1, -1,-1, 1, 1, -1, -1,-1,1)
x2 <- c(1,1,1,1,1,1,-1,-1,-1,1,-1,1,1,1,-1)
s1 <- as.matrix(c(x1))
print(s1)
s2 <- as.matrix(c(x2))
print(s2)
t1 = 1
t2 = -1
w = (t1*(s1) + t2*(s2))
print(w)


if((image <- sum(s1*w)) >= 0)
{
  print("This is image C")
}
if((image <- sum(s2*w) )< 0)
{
print("This image is D")
}



