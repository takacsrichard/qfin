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

# hw 83
hw83 <- sum(grepl(" ", state.name))
hw83

# hw 87
hw87 <- sum(grepl("red", colors()))
hw87

# hw 85
hw85_s_count <- lengths(regmatches(state.name, gregexpr("s",state.name, ignore.case=TRUE)))
hw85 <- state.name[hw85_s_count>3]
hw85

# hw 86
hw86 <- state.name[grepl("([aeiou])\\1", state.name, ignore.case=TRUE)]
hw86
