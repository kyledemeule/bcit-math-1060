library("data.table")
salary = fread("http://mathweb.bcit.ca/data/salary.csv")

y = salary$year
s = salary$salary
n = length(y)

plot(y, s)
plot(y, log(s))

ls = log(s)
r = cor(y, ls)
b1 = r * (sd(ls) / sd(y))
b0 = mean(ls) - (b1 * mean(y))

test_points = c(0, 2000)
points(test_points, (test_points * b1) + b0, type="l")

ta2 = qt(0.95, df=n - 2)
se = sqrt(sum((ls - (y * b1 + b0))^2) / (n - 2))
sb1 = (1 / sqrt(n-1)) * (se / sd(y))
cat(b1 - ta2 * sb1 , "< slope < ", b1 + ta2 * sb1)

cat("2016 guess: ", exp(2016 * b1 + b0))