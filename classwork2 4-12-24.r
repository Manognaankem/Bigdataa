# define If or else
x <- 0
count <- 0
print(count)

#Selection
if( x>= -1 & x <=1){
  count <- count+1
} else{
  count <- count-1
}
print(count)

#  while Loop 
counter <-1
while(counter<12){
  # use print inside a loop
  print(counter)
  counter <- counter +1
}
#For loop
for (i in 1:5){
  print("Hello R")
}

for (m in 5:10)
  print(m)

#square root loop
for (x in 1:20) {
  print(sqrt(x))
}
#Vectors

myFirstVector <- c( 2, 5, 7, 1, 4)
myFirstVector
typeof(myFirstVector)

mySecondVector <- c("2","4","8")
mySecondVector
typeof(mySecondVector)
print(mySecondVector)

myThirdVector <- c( T, F, T)
myThirdVector
typeof(myThirdVector)

# Index Vectors
X <- c(3,4,5)
Y <- c(X,X,X,X)
Y

# Vector using a Range
V <- 1:10
S <- -5:30
T <- 10:1
Mix <- c( 3:6, 24, 0:4, 31, 9:7)


#Vector using Seq Function
A <- 1:10
A <- seq(1,10)

#seq has a nice extra argument by:
c <- seq(0,12, by=3)
d <- seq(10, -10, by=-2)

#Another possible argument length:
D <- seq(1,10, length=20)
E <- seq(0,1, length=15)

#vector using rep function

A <- rep("Hello", 5)
B <- rep(A,2)
C <- rep(1:5,4)
D1 <- rep(c("A","B","C"), 3)
D2 <- rep( c("A","B","C"), each=3)
D3 <- rep( c("A","B","C"), c(2,1,3) )

#mixed to single mode 
T <- c("Hello", 3, 67L, 6.8)
o <- c(6.8)
typeof(o)
typeof(T)

#Vector Attributes:

X <- c(3,5,8)
typeof(X)
length(X)

#partial vectors
w <- c ("a", "b", "c", "d", "e")

w[1]
w[3]
w[-1]                            
w[-3]
w[c(1,3,5)]                    
w[c(-2,-4)]                 
w[c(-3:-5)]                   
w[1:3]     

#sorting a vector
A <- c(5,-2,51,42,-76,42)
B <- sort(A)
A
B

#Arithmetic
X <- c( 1, 2, 3)
Y <- c( 4, 5 , 6 )
Z <- X + Y
X
Y
Z


#Different length vectors

X <- c( 1, 2, 3)
Y <- c( 4, 5 , 6, 7, 8, 9 )
Z <- X + Y
X
Y
Z

#more math functions

X <- c( 34,56,22,11,65,45)
Y <- mean(X)
Z <- min(X)
W <- max(X)
S <- sum(X)
V <- prod(X)

Tax <- c(32,56,44,76,89,24)

for (i in 1:6)
  if (Tax[i] < 50)
    print(i)

for (i in Tax)
  if (i <50)
    print(i)

# changing fahrenheit to celsius
c <-(45-32)*5/9
print(c)
c <-(77-32)*5/9
print(c)
c <-(20-32)*5/9
print(c)
c <-(19-32)*5/9
print(c)
c <-(120-32)*5/9
print(c)
c <-(101-32)*5/9
print(c)
c <-(212-32)*5/9
print(c)

