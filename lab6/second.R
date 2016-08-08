# confidence interval for population mean
xb0 = 21.3 # sample mean
s0 = 0.7 # sample s.d.
n = 4 # sample size
alpha = 0.05 # significance level
ns = 1000 # number of samples to be generated
population = rnorm(3000, mean=xb0, sd=s0) # generate normally distributed random numbers
hist(population, breaks=30)

s=rep(0, ns) # initialize s
for(i in seq(1, ns)) s[i] = sd(sample(population, n)) # repeat ns times
hist(s, breaks=30) # display sampling distribution

# chi square
sc=rep(0, ns) # initialize s
for(i in seq(1, ns)) sc[i] = ((n-1) * sd(sample(population, n))^2) / (s0^2) # repeat ns times
hist(sc, breaks=30) # display sampling distribution

left = round(ns*alpha/2); right = round(ns*(1-alpha/2))
sc.sorted = sort(sc, method="quick")
sl = sc.sorted[left]; sr = sc.sorted[right]
cat("Simulation: ", s0^2 / sr, " < pop.sd < ", s0^2/sl)
