# homework 3
# richard takacs
# matr.nr 12313036

# hw 48
hw48 <- function(base, prec, emin, emax){
  if(emax<emin) stop("emax must be leq than emin")
  2*(base-1)*base^(prec-1)*(emax-emin)
}
hw48(2, 1, -1, 2)

hw48s <- function(base, prec, emin, emax) base^emin
hw48l <- function(base, prec, emin, emax) base^(emax)*(1-base^(-prec))
hw48s(2, 1, -1, 2)
hw48l(2, 1, -1, 2)

# hw 53
abs(3*(4/3-1)-1)
.Machine$double.eps

# hw 54
# it is because -0.999 [16x] > eps/2 but 1-0.999 [17x]<eps/2 when considering where the number rounds to.
.Machine$double.eps / 4
x <- c(0,7,8)
x[0.9999999999999999]
x[0.99999999999999999]
0.9999999999999999 == 1
0.99999999999999999 == 1

# hw 55
hw55 <- function(){
  e_plus<- 1
  while(1+e_plus/2!=1) e_plus <- e_plus/2
  e_min<- 1
  while(1-e_min/2!=1) e_min <- e_min/2
  cat("1+e, e= ", e_plus, "; 1-e, e=",e_min)
  
}
hw55()
.Machine$double.eps
.Machine$double.eps/2
# hw 63
# classic example exploiting that large + small loses the small part, so the order of
# operations changes the result
a <- 1.0
b <- 1e16
c <- -1e16
x <-(a+b)+c
y <- a+(b+c)
x == y

# hw 64
x <- 1e16
y <-(log(exp(x)))
z <- (exp(log(x)))
z == y

hw65 <- function(){
  ks <- 1:20
  formula <- sapply(ks, function(k) (1 + 1/10^k)^(10^k))
  err <- abs(formula-exp(1))
  data.frame(k=ks, formula=formula, err=err)
}
hw65()

# hw 66
hw66n <- function(n) (1+1/n)^n
hw66h <- function(h) (1+h)^(1/h)
hw66n(1e7)==hw66h(1e-7)
exp(1)

# hw 67
# todo check if number is natural
hw67 <- function(n){
  if(n<=1 || n %% 1 != 0) stop("n should be >1 natural number")
  (factorial(n-1)+1)%%n==0
}
for (i in 2:25){cat(i, hw67(i), "\n")}

# not a good way because the factorial overflows around n = ~ 18-19
factorial(19)
2^53-1
#2^53 overflows around

# hw 68
# lim_{x->0} e^x/1 = e^0 = 1.
hw68<- function(x) (exp(x)-1)/x
k <- 1:15
data.frame(k, hw68(10^(-k)))

hw68_2 <- function(x) (exp(x)-1)/log(exp(x))
data.frame(k, hw68_2(10^(-k)))

# hw 69
hw69 <- function(f, x, h) (f(x+h) - f(x))/h
k <- 10^(-(1:15))
hw69_1 <- sapply(k, function(h) hw69(tan, 1, h))
hw69_2 <- 1+tan(1)^2
data.frame(h=k, hw69_1, hw69_2)

# hw 70
# TODO

# hw 71
hw71_1 <- function(x) 1/(1-x) - 1/(1+x)
hw71_2 <- function(x) 2*x/(1-x^2)
k <- 1e-16
hw71_1(k)
hw71_2(k)
print(hw71_1(k)-hw71_2(k))
