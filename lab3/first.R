# Normal Distribution
x = seq(-5, 5, by=0.05)
density01 = dnorm(x, mean=0, sd=1)
plot(x, density01, type="l")
densityA = dnorm(x, mean=0, sd=2)
points(x, densityA, type="l", col="red")

densityA = dnorm(x, mean=1, sd=3)
points(x, densityA, type="l", col="blue")

# Probability: Area under Normal distribution
z1 = -2
z2 = 1.5

z = seq(-4, 4, by=0.05)
density = dnorm(z)
plot(z, density, type="l")

z_range = seq(z1, z2, by=0.05)
polygon(c(z1, z_range, z2), c(0, dnorm(z_range), 0), col="red")
points(c(-4, 4), c(0, 0), type="l")

# p(z < 1)
pnorm(1)
# p(z > 1)
1 - pnorm(1)
# graphing it
z_over_one = seq(1, 4, by=0.05)
plot(z, density, type="l")
polygon(c(1, z_over_one, z2), c(0, dnorm(z_over_one), 0), col="blue")
points(c(-4, 4), c(0, 0), type="l")

# p(-1 < z < 1)
pnorm(1) - pnorm(-1)
# p(-2 < z < 2)
pnorm(2) - pnorm(-2)
# p(-3 < z < 3)
pnorm(3) - pnorm(-3)

# Sampling distribution
population = runif(1000000, 0, 10)
mu = mean(population)
sigma = sd(population)

ns = 5
N = 1000
xbars = rep(0, N)
for(i in 1:N) {
  xbars[i] = mean(sample(population, ns, replace=TRUE))
}
hist(xbars, xlim=c(mu - 3 * sigma, mu + 3 * sigma), breaks=50, probability=TRUE)
# superimpose normal distribution
z = seq(0, 10, by=0.05)
points(z, dnorm(z, mean=mu, sd=sigma/sqrt(ns)), type="l", col="red")