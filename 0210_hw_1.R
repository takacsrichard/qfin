# hw 1
# name: richard takacs
# matrnr: 12313036
# class 0210 
setwd("~")
# ------------------- HW 1 ----------------------- 
# hw1
hw1 <- 170166719 %% 31079

# hw2
r <- 1.08
nvals <- 1:100
sum1 <- numeric(100)
sum2 <- numeric(100)
for (n in nvals) {
  sum1[n] <- sum(r^(1:n))
  sum2[n] <- (r^(n+1)-1)/(r-1)-1 #sum of x1....x_n==(x^(n+1)-1)/(x-1)-1  
  if (n %in% c(10, 20, 30, 40)){
    cat("n=",n," sum: ",sum1[n]," formula: ", sum2[n], "\n")
}}
sum_all_n <- (r^(nvals)-1)/(r-1)*r # sum of x1....x_n==(x^n-1)/(x-1)*x   
all.equal(sum1,sum_all_n)

# hw3
nvals <- 1:100
for (n in c(100,200,400,800)){
  sum1 <- sum(1:n)
  sum2 <- n*(n+1)/2
  cat("n=",n," sum: ",sum1," formula: ", sum2, "\n")
}
sum_all_n <- nvals*(nvals+1)/2 
sum_all_n

# hw4
nvals <- 1:100
for (n in c(200,400,600,800)){
  sum1 <- sum((1:n)^2)
  sum2 <- (n*(n+1)*(2*n+1))/6
  cat("n=",n," sum: ",sum1," formula: ", sum2, "\n")
}
sum_all_n <- nvals*(nvals+1)*(2*nvals+1)/6
sum_all_n

# hw5
for (n in c(500,1000,2000,4000,8000)){
  sn <- sum(1/(1:n))
  cat("n=",n," sum: ",sn," logn: ", log(n), "ratio: ", sn/log(n), "diff: ", sn-log(n), "\n")
}
# 0.57721 is the euler constant, which is known to be the limit
# test of difference between harmonic series and log(n):
n_test <- 10^(0:7)
for (n in n_test){
  sn <- sum(1/(1:n))
  cat(n, ":", sn-log(n)," ")
}

# hw6
#?rep
hw6_1 <- rep(0:4, each=5)
hw6_1
hw6_2 <- rep(1:5,5) # times
hw6_2

# hw7
hw7 <- lapply(1:5, function(i) i:(i+4))
hw7 <- unlist(hw7)
hw7
# if we had to use rep and seq
hw7_2 <-  rep(seq(1:5), 5)+rep(0:4, each=5) #first part is 12345 5 times, second part is 000001111122222...
hw7_2

# hw8
cat("input number. must be natural number")
n <- as.integer(readline())

hw8 <- function(n){
  rep(1:n, 1:n) #repeat 1:n for 1:n times
}
hw8(4)

# hw 9
numbers <-  c(3,5,8,10,12)
"numbers" %in% ls()
dump("numbers", file="numbers.R")
rm(numbers)
"numbers" %in% ls()
source("numbers.R")
numbers
# file.remove("~/numbers.R")

#hw 10
# r has LETTERS constant
# alternatively LETTERS <- c("A", "B", "C", ... "Z")
LETTERS
hw10 <-  function(path){ # from cwd
  dump("LETTERS", file=path)
} 
# hw10("filename.R")
# "filename.R" %in% list.files()

# hw 11
hw11 <- function(x){
  ifelse(x<=3, 3*x+2, 2*x-0.5*x^2)
}
plot(hw11, from=0, to=6, n=100)

# hw 12
hw12 <-  function(){
  n<-as.numeric(readline(prompt="enter number: "))
  ifelse(n>0, "positive", ifelse(n<0, "negative", "zero"))
}

hw12_1 <- function(x){
  ifelse(x>0, "positive", ifelse(x<0, "negative", "zero"))
} 

hw13 <-  function(){
  cat("in this number bigger than pi in absolute value?")
  # if we do not care about builtin pi constant: 
  # n<-as.numeric(readline(prompt="enter number: ")) 
  n<-eval(parse(text=readline(prompt="enter number: ")))
  abs(n)>pi
}

hw13_1 <-  function(x){
  abs(x)>pi
}
# hw13_1(-pi+0.01)
# hw 14
hw14_geom <- function(x,y) sqrt(x*y)
hw14_harmo <- function(x,y) (2*x*y)/(x+y)
# hw14_geom(10,5.5)
# hw14_harmo(10,5.5)

# hw 15
#?nchar
hw15 <- function(x,y) if(nchar(x)<nchar(y)) x else y
#hw15("test", pi)
#hw15("test", "test1")

# hw 16
hw16 <-  function(x){
  for(i in 1:nchar(x)){
    print(x)
  }}
#hw16(pi)

# hw17
hw17 <-  function(n){
  if (n != as.integer(n) || n<=0) stop("x must be natural number")
  cat(n:1, sep="\n")
}
#hw17(10)
#hw17(pi)

# hw 18
hw18 <- function(x,y){
  if (y==0) stop("y must be nonzero")
  if(x%%y==0){
    cat(x, "and", y, "are divisible")
  }
  else{
    cat(x, "and", y, "are not divisible")
  }
    
}
#hw18(5,2)
#hw18(pi, pi+0.000000000000000000001)

# hw19
hw19 <- function(x){
  if (x != as.integer(x) || x<=0) stop("x must be natural number")
  ch1 <- as.numeric(strsplit(as.character(x),"")[[1]])
  sum(ch1)
}
# if we want to use %% and %/% we can
hw19_1 <-  function(x){
  if (x != as.integer(x) || x<=0) stop("x must be natural number")
  remainder <- 0
  while(x>0){
    remainder <- remainder+x %% 10
    x <-x%/%10 #"eliminate" last digit
  }
  remainder
}

# hw 20
hw20 <- function(x){
  if (any(x != as.integer(x) | x<=0)) stop("must be a seq of natural numbers")
  sum(nchar(as.character(x)))
}
#hw20(pi)
#hw20(seq(0,10,by=2))
#hw20(seq(1,12,by=2))
#hw20(c(2,3,4444))
#hw20(-2)
#hw20(0.001)
#hw20(rep(10,10))

#hw 21
hw21 <-  function(x,y,z){
  if (!is.numeric(x) || !is.numeric(y) || !is.numeric(z)) stop("each arg must be numeric")
  sorted <- sort(c(x,y,z))
  sorted[2]^2+sorted[3]^2
}
hw21(2,3,4)
hw21(0,0,0)
#hw21(c(2,3,4))
#hw21(2,pi,"asd")

# hw 22
# assumiung inputs are valid
# we have to write a helper because 0.333 is not stored cleanly in binary
#   and because math libraries compute x^y as e^(y*ln(x))
# since (-q/2 +- sqrt(D)) can be negative, (negative)^(1/3) is NaN
hw22 <- function(a,b,c){
  p <- (3*b-a^2)/3
  q <- (2*a^3)/27 - (a*b)/3 +c
  D <-  (p/3)^3 + (q/2)^2
  if (D<0) stop("Discr is negative")
  cuber <-  function(x) sign(x) * abs(x)^(1/3)
  x <- -a/3 + cuber(-q/2+sqrt(D))+cuber(-q/2-sqrt(D))
  x
}
hw22(1,1,1)
hw22(0,0,-1)

# hw 23
# n should be a natural number or 0.
# the function will return 0 for negative numbers (because anything * 0 is 0)
#   which is correct for the RHS of the exercise, although n! for n<0 is undefined
# (factorial overwrites builtin factorial)
factorial <-function(n){
  if(n==0) return(1)
  prod(1:n)
}
# a)
factorial(10)
factorial(50)
factorial(100)
factorial(1000)

# b)
# \binom{n}{k} == \frac{n!}{k!(n-k)!}
bico <- function(n,m){
  factorial(n)/(factorial(m)*factorial(n-m))
}
bico(4,2)
bico(50,20)
bico(5000,2000)

# c)
# considering that \ln(\binom{n}{m}) can be computed by \ln(n!)-\ln(m!)-\ln((n-m)!)
# since ln(n)=ln(1*2*3*...*n)=ln(1)+ln(2)...+ln(n)=sum(ln(1:10))
bico_c <-  function(n,m){
  exp(sum(log(1:n))-sum(log(1:m))-sum(log(1:(n-m))))
}
bico_c(4,2)
bico_c(50,20)
bico_c(5000,2000)

# hw 24
#straightforward implementation:
#?gamma
rho_n <-  function(n){
  gamma((n-1)/2)/(gamma(1/2)*gamma((n-2)/2))
}
rho_n(2000)
# this fails because gamma overflows to Inf as the gamma function == (n-1)!, which becomes very large for n=2000
# using lgamma, we can use exp(lgamma()) ≈≈ gamma() by the definition of ln
# all.equal(exp(lgamma(n)), gamma(n))==TRUE
# by log transformations, ln(rho_n)=ln(gamma(X))-ln(gamma(Y))-ln(gamma(Z))
rho_n_l<-function(n){
  exp(lgamma((n-1)/2)-lgamma(1/2)-lgamma((n-2)/2))
}

all.equal(exp(lgamma(20)), gamma(20))
all.equal(rho_n_l(50), rho_n(50))
all.equal(rho_n_l(2000), rho_n(2000))
rho_n_l(2000)

# the limit of rho_n / sqrt(n)
rho_n_l(10000)/sqrt(10000)
curve(rho_n_l(x)/sqrt(x), from=0, to=1000)
# it converges to a number around 0.3989, i.e. 1/sqrt(2pi)
1/sqrt(2*pi)
# this happens to be the max height of the standard normal density function


# ------------------- HW 2 -----------------------

# hw 25
# assuming input is numeric and positive
logstar <-function(x){
  counter <- 0
  x <- as.numeric(x)
  while (x>=1){
    x <- log(x)
    counter <- counter+1
  }
  return(counter)
}
logstar(123)

# hw 26
# assuming gcd is undefined for rational numbers
# todo check that link in pdf again
gcd <- function(a,b){
  if (a!= as.integer(a) || b!= as.integer(b)) stop("a and b must be integers") 
  if (b==0) stop("b can't be 0")
  if (a%%b==0) {return(b)}
  else {
    return(gcd(b,a%%b))
  }
}


#hw 27
hw27<- function(x){
 tmp <- 1:x
 rep(tmp%%2, times=tmp)
}

# hw 28
#solution2: convert whole to logical vectors and sum()
hw28 <- function(x){
  tmp <- 0
  for (i in x){
    if(i%%2==1)
      tmp <-  tmp+1
  }
  return(tmp)
}

# hw 29
# if we cant use homework26, we can reimplement it accordingly
hw29 <- function(x,y){
  if(gcd(x,y)==1) return(TRUE) else return(FALSE)
}

#hw 30
hw30 <- function(x){
  if (x<=1 || x!=as.integer(x)) stop("negative, 0, 1, and non-whole numbers are not considered prime")
  if (x==2) return(TRUE)
  for(i in 2:(as.integer(sqrt(x))+1)){
    if (x%%i==0) {
      return(FALSE)}
  }
  return(TRUE)  
}


