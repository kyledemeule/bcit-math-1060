# flipping a coin
n = 1:1000
P = rep(0, times=length(n))
for(m in n) {
  x = sample(c(0, 1), m, replace=TRUE)
  P[m] = sum(x) / m
}
plot(n, P)
P[length(n)]

# rolling a die and getting 6
n = 1:1000
P = rep(0, times=length(n))
for(m in n) {
  x = sample(c(0, 0, 0, 0, 0, 1), m, replace=TRUE)
  P[m] = sum(x) / m
}
plot(n, P)
P[length(n)]

# rolling a die and getting 6 and getting heads
n = 1:1000
P = rep(0, times=length(n))
for(m in n) {
  x = sample(c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0), m, replace=TRUE)
  P[m] = sum(x) / m
}
plot(n, P)
P[length(n)]

# Binomial distribution
n = 100
p = 1/2
# single coin toss
ns = 100000
x = rep(0, times=ns)
for(i in 1:ns) {
  x[i] = sum(sample(c(0, 1), n, replace=TRUE))
}
hist(x)
mean(x)
n*p
sd(x)
sqrt(n*p*(1-p))

# Binomial distribution different coin
n = 100
p = 1/3
# single coin toss
ns = 100000
x = rep(0, times=ns)
for(i in 1:ns) {
  x[i] = sum(sample(c(0, 0, 1), n, replace=TRUE))
}
hist(x)
mean(x)
n*p
sd(x)
sqrt(n*p*(1-p))
# What changed? peak went from ~50 (with 100 flips per sample)
# to ~33 when the probability of heads went to 1/3