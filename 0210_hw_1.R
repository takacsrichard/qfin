# hw 1
# name: richard takacs
# matrnr: 12313036
# class 0210 
setwd("~")

# hw1
hw1 <- 170166719 %% 31079

# hw2
r <- 1.08
nvals <- 1:100
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
hw6_1 <- rep(0:4, each=4)
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
hw15 <- function(x,y) if(nchar(x)>nchar(y)) x else y
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
