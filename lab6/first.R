# confidence interval for population mean
xb0 = 21.3 # sample mean
s0 = 0.7 # sample s.d.
n = 4 # sample size
alpha = 0.05 # significance level
ns = 1000 # number of samples to be generated
population = rnorm(3000, mean=xb0, sd=s0) # generate normally distributed random numbers
hist(population, breaks=30)

xbar=rep(0, ns) # initialize xbar
for(i in seq(1, ns)) xbar[i] = mean(sample(population, n)) # repeat ns times
hist(xbar, breaks=30) # display sampling distribution

left = round(ns*alpha/2); right = round(ns*(1-alpha/2))
xbar.sorted = sort(xbar, method="quick")
cat("Simulation: ", xbar.sorted[left], " < mu < ", xbar.sorted[right])

# Theoretical result for comparison
E = qnorm(1-alpha/2)*s0/sqrt(n) # Margin of error
cat("Theoretical: ", xb0 - E, " < mu < ", xb0 + E)