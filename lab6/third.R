# confidence interval for population mean
xb1 = 21.3 # sample mean
s1 = 0.7 # sample s.d.
xb2 = 20.3
s2 = 0.5
n = 4 # sample size
alpha = 0.05 # significance level
ns = 1000 # number of samples to be generated
population1 = rnorm(3000, mean=xb1, sd=s1) # generate normally distributed random numbers
hist(population1, breaks=30)
population2 = rnorm(3000, mean=xb2, sd=s2) # generate normally distributed random numbers
hist(population2, breaks=30)

dbar=rep(0, ns) # initialize xbar
for(i in seq(1, ns)) dbar[i] = mean(sample(population1, n)) - mean(sample(population2, n)) # repeat ns times
hist(dbar, breaks=30) # display sampling distribution

left = round(ns*alpha/2); right = round(ns*(1-alpha/2))
dbar.sorted = sort(dbar, method="quick")
cat("Simulation: ", dbar.sorted[left], " < mu < ", dbar.sorted[right])

# Theoretical result for comparison
E = qnorm(1-alpha/2)*sqrt((s1^2/n)+(s2^2/n)) # Margin of error
cat("Theoretical: ", (xb1-xb2) - E, " < mu < ", (xb1-xb2) + E)
