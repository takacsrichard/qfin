# homework 4
# richard takacs
# matr.nr 12313036

# hw 79
# we get the largest n where the function(n)==function(n+1)
hw79_n <- function(n) sum(1/10^(1:n))
n <- which(sapply(1:30, function(n) hw79_n(n) == hw79_n(n+1)))[1]
hw79 <- hw79_n(n)
hw79
n
1/9

# hw 73
hw73 <- function(j,x) {
  integrate(function(theta) exp((1-theta)*x)*theta^(j-1),0, 1)$value / factorial(j-1)
}
hw73_a <- function(j,x) (phi(j-1, x)-phi(j-1, 0))/x
hw73(10, 2)
hw73_a(10, 2)
hw73(10, 2) == hw73_a(10, 2)
hw73(10, 0.0001) == hw73_a(10, 0.0001)
# TODO

# hw 74
# scale by the max absolute component before squaring, to avoid overflow/underflow
hw74 <- function(x) {
  sqrt(sum((x)^2))
}
hw74(c(3, 4))
hw74(c(1e300, 1e300))
# this results in infinity. To avoid this, we can divide and then multiply with a big enough term. for simplicity, let this be the maximum x.

hw74_2 <- function(x) {
  xmax <- max(abs(x))
  if (xmax==0) return(0)
  sqrt(sum((x/xmax)^2))*xmax
}
hw74_2(c(3, 4))
hw74_2(c(1e300, 1e300))


# hw 78
hw78 <- function(x) {
  if(abs(x) < 0.1) return(x) #if abs leq than 0.1 rad
  3*hw78(x/3)-4*hw78(x/3)^3
}
hw78(1)
sin(1)
hw78(pi)
sin(pi)

# hw 79
# we get the largest n where the function(n)==function(n+1)
hw79_n <- function(n) sum(1/10^(1:n))
n <- which(sapply(1:30, function(n) hw79_n(n) == hw79_n(n+1)))[1]
hw79 <- hw79_n(n)
hw80
log(2)

# hw 80
k <- 0:1e7
hw80 <- sum((-1)^(k)/(k+1))
hw80
log(2)

# hw 81
# this is known to be 1/(4n-3)(4n-1)
k <- 1:1e7
hw81 <- sum(1/((4*k-3)*(4*k-1)))
hw81
pi/8

# hw 83
hw83 <- sum(grepl(" ", state.name))
hw83

# hw 84
hw84_vowels <- lengths(regmatches(state.name, gregexpr("[aeiou]", state.name, ignore.case = TRUE)))
hw84_consonants <- lengths(regmatches(state.name, gregexpr("[^aeiou ]", state.name, ignore.case = TRUE)))
hw84 <- state.name[hw84_vowels > hw84_consonants]
hw84

# hw 85
hw85_s_count <- lengths(regmatches(state.name, gregexpr("s",state.name, ignore.case=TRUE)))
hw85 <- state.name[hw85_s_count>3]
hw85

# hw 86
hw86 <- state.name[grepl("([aeiou])\\1", state.name, ignore.case=TRUE)]
hw86

# hw 87
hw87 <- sum(grepl("red", colors()))
hw87

# hw 88
# TODO

# hw 89
wc <- function(file) {
  words <- unlist(strsplit(gsub("[[:punct:]]", "",paste(readLines(file),collapse=" ")), "\\s+"))
  words <- words[words!=""]
  length(unique(tolower(words)))
}
wc("package.json")

# hw 90
hw90 <- function(file, word) {
  word=tolower(word)
  text <- gsub("[[:punct:]]","",paste(readLines(file),collapse=" "))
  words <- tolower(unlist(strsplit(text,"\\s+")))
  sum(words==word)
}
hw90("package.json", "package")

# hw 92
hw92 <- function(sub) {
  state <- sample(state.name, 1)
  chars <- strsplit(tolower(sub), "")[[1]]
  cat(all(chars %in% strsplit(tolower(state), "")[[1]]), "\n")
  cat("state:", state)
  
}
hw92("A")

# hw 94
#TODO

# hw 97
# TODO