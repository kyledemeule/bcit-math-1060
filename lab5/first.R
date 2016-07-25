library(datasets)
x = chickwts$weight
hist(x)

xbar = mean(x)
s = sd(x)
n = length(x)

# Question (a)
a  = 1 - 0.9

# Confidence interval for mu
tc = qt(a/2, df=n-1, lower.tail=FALSE)
E = (tc * s) / sqrt(n)
cat(xbar-E, ' < mu < ', xbar+E)

# Confidence interval for sigma
chi2R = qchisq(1-(a/2), df=n-1)
chi2L = qchisq(a/2, df=n-1)
chi2R = qchisq(1-(a/2), df=n-1)
sigmaLower = s * sqrt((n-1)/chi2R)
sigmaUpper = s * sqrt((n-1)/chi2L)
cat(sigmaLower, ' < sigma < ', sigmaUpper)

# Question (b)
mu0 = 270
a = 0.05
t = (xbar - mu0) / (s / sqrt(n))
tc = qt(a, df=n-1, lower.tail=TRUE)
pval = pt(t, df=n-1)
cat('t=', t, ', tc=', tc, ', pval=', pval)

t.test(x, mu=mu0, alternative="less", conf.level = 1-a)
