# Homework 2
# Richard Takacs
# Matr.Nr 12313036

# hw25
logstar <- function(x){
  n <- 0
  x <- as.numeric(x)
  while(x >= 1){
    x <- log(x)
    n <- n+1
  }
  return(n)
}
logstar(123)
logstar(1)
logstar(0.5)
logstar(pi)
logstar(1111111)
logstar(-1)

# hw 26
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

# hw 31
# using hw30
hw31 <- function(){
  cntp <-0
  i <-2
  while(cntp !=7){
    if(hw30((2^i)-1)) {
      cat((2^i)-1,"\n")
      cntp <- cntp+1
    }
    i <- i+1
  }
}

#hw 32
hw32 <- function(x){
  tmp <- 0
  if(x==1) stop("1 is not considered a perfect number")
  for(i in 1:(x-1)){
    if(x%%i==0){
      tmp = tmp +i
    } 
  }
  ifelse(tmp==x, return(TRUE), return(FALSE)) 
}
cnt <- 0
i <- 2
while(cnt != 4){
  if(hw32(i)) {
    cat(i,"\n")
    cnt= cnt+1}
  i = i+1
}
rm(i, cnt)

#hw33
hw33 <- function(x){
  if (x<=1) stop("function is only defined for positive integers that are larger than 1")
  maxcnt <- 0
  for (j in 1:x){
    tmpcnt <- 0
    for(i in 1:j){
      if(j%%i==0){
        tmpcnt=tmpcnt+1
      } 
    }
    if(tmpcnt>maxcnt){
      maxcnt = tmpcnt
      if(j==x) return(TRUE)
    }
  }
  return(FALSE)
}
# 
# hw 34
hw34 <- function(n){
  s <- 0
  while(n!=1){
    n <- if(n%%2==0) n/2 else 3*n+1
    s <- s+1
    if(s==10000000) stop("safety condition triggered")
  }
  return(s)
}
j <- 0
maxlen <- 0
maxlen_i <- 0
for(i in 1:100000){
  # if wo numbers have the same sequence length, take first one
  x <- hw34(i)
  if(x>maxlen){
    maxlen=x
    maxlen_i=i}
  j = j+as.logical(x)
}
maxlen_i


# hw 35
# we can build up the binary using %% and %/%
hw35 <- function(x){
  bits <- c()
  while(x>0){
    bits <- c(x%%2, bits)
    x <- x%/%2
  }
  return(bits)
}
hw35(6)

hw35_inverse <- function(n){
  x <- 0
  for(i in n){
    x <- 2*x+i 
  }
  return(x)
}
hw35_inverse(c(1,1,1,0))


# hw36
# LCM is derived as a/gcd()*b
hw36 <- function(a,b){
  (a/gcd(a,b))*b
}
hw36(0,5)
hw36(1,5)
hw36(987652321,123712218)

# hw37
# calling the function itself
hw37 <- function(m,n){
  if(m == 0) {
    return(n+1)}
  if(n == 0 && m>0) {return(hw37(m-1, 1))
  }
  hw37(m-1,hw37(m, n-1))
}
hw37(2,3)
hw37(3,2)

# hw 38
# pascal triangle is basically "n choose k" for all k 0:k
hw38 <- function(x){
  choose(x, 0:x)
}
hw38(4)
hw38(10)

# hw 39
hw39 <- function(s0, u, d, n){
  s <- list()
  for (x in 1:n) {
    sn <- c() 
    for (i in 0:x) {
      v <- s0*(u^i)*(d^(x-i))
      sn <- c(sn, v)
    }
    s[[x]] <- sn
  }
  return(s)
}
hw39(60,3,3,5)
# 
hw39_apply <- function(s0,u,d,n) {
  s <- lapply(1:n, function(n) {
    i <- 0:n
    s0*(u^i)*(d^(n-i))
  })
  
  return(s)
}

# lapply is better (?)

# 40
hw40 <- function(n){
  sum <- 0
  for(r in 1:n){
    for(c in 1:r){
      sum <- sum+(c^2)/(5+10*r^3)
    }
  }
  return(sum)
}
hw40(2)

# we treat r as the row and c as cols., then the sum X_ij can be calculated via matrix operations
# ?lower.tri
hw40_rc <- function(n){
  mm <- matrix(0, n, n)
  # every number in 1:n has a row and col value. e.g. for row 1 col 1, col(mm)==row(mm)==1 
  res <- col(mm)^2/(5+10*row(mm)^3)
  # since res returns a matrix, we have to sum the values where c<=r, as per the sums
  # diag true includes the diagonal
  return(sum(res[lower.tri(res, diag=TRUE)]))
}
hw40_rc(2)

#hw 41
#TODO

# hw 42
hw42 <- function(l) {
  unsigned <- sum(l*2^((length(l)-1):0))
  if(l[1] == 1){unsigned - 2^length(l)} else {unsigned}
}

ones <- function(x){
  hw42(rep(1,x))
}

zeros <- function(x) {
  if(x<2) stop("error")
  hw42(c(1, rep(0, x-2), 1))
}

# tests
ones(8)
ones(32)
zeros(8)
zeros(32)

# hw 43
.Machine$integer.max+
  .Machine$integer.max*(-1L) # the mathematical minimum value is -2147483648
# i.e .Machine$integer.max*(-1L)-1L
# However, R treats that as overflow even though its valid

# hw 44
# TODO

# hw 45
hw45 <- function(x, k=32){
  ks <- as.integer(k)
  if(x<(-2^(k-1)) || x > 2^(k-1)-1) stop("too small/large x")
  if(x<0) x<-x+2^k #2^k-x since x<0
  for(i in k:1){
    ks[i] <- x%%2
    x <- x%/%2
  }
  ks
}
hw45(-6)

hw45_inv <- function(x){
  l <- length(x)
  res<- sum(x*2^((l-1):0))
  if(x[1]==1) res <- res-2^l
  return(res)
}
hw45_inv(c(0,1,1,0))


# Dividing a fraction  by the gcd of its numer and denom reduces it to its simplest form
hw46 <- function(p, q){
  if(q==0) stop("denom cant be 0")
  gc <- gcd(abs(p), abs(q))
  if(gc == 0) gc <- 1 #gcd(0,0) results in 0, breaking the divisions
  p <- p/gc
  q <- q/gc
  # to have negative sign in the front:
  if(q < 0){
    p <- -p
    q <- -q
  }
  c(p, q)
}
hw46(19, -8)

#if we assume numer and denom takes the results of hw46 (i.e. c(a,b))
numer<-function(x) x[1]
denom<-function(x) x[2]

# hw 47
add_rat <- function(x,y){
  # a/b+c/d = (ad+cb)/(bd)
  p <- numer(x) * denom(y) + numer(y) * denom(x)
  q <- denom(x) * denom(y)
  hw46(p, q)
}
add_rat(c(1,2), c(1,3))

mul_rat <- function(x, y){
  # (a/b)(c/d) = (ac)(1/(bd)) = (ac)/(bd)
  hw46(numer(x) * numer(y), denom(x) * denom(y))
}
mul_rat(c(1,2), c(2,3))
