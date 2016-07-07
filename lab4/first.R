install.packages("data.table")
library("data.table")
bitcoin = fread("http://mathweb.bcit.ca/data/bitcoin.csv")

confidenceIntervals = function(rawData, range) {
  cleanData = ifelse(rawData < 3000, rawData, 0)
  x = cleanData[range]
  plot(range, x, type='o')
  mu = mean(x)
  s = sd(x)
  n = length(x)
  cat("mean =", round(mu, digits=2), " sd =", round(s, digits=2), '\n')
  xg = seq(mu - 3 * s, mu + 3 * s)
  zg = (xg - mu) / s
  
  # 95% confidence interval using Normal distribution
  En = (qnorm(1 - (1-0.95)/2) * s) / sqrt(n)
  cat("Normal distribtion: E=", En, ", ", mu - En, "<mu<", mu + En, '\n')
  
  # 95% confidence interval t distribution
  Et = (qt(1 - (1-0.95)/2, df=n-1) * s) / sqrt(n)
  cat("t-distribution: E=", Et, ", ", mu - Et, "<mu<", mu + Et, '\n')
  
  # graphs of normal and t-distribution
  plot(xg, dnorm(zg), type="l", xlab="x", ylab="Prob.density")
  points(xg, dt(zg, df=n-1), type="l", col="green")
}

confidenceIntervals(bitcoin$Close, 1295:1300)
