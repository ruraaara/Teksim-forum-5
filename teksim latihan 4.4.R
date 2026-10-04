library(MASS)

set.seed(67)

mu <- c(2, 3)
Sigma <- matrix(c(9, 6,
                  6, 16), nrow = 2, byrow = TRUE)

data <- mvrnorm(n = 1000, mu = mu, Sigma = Sigma)
colnames(data) <- c("X", "Y")

head(data)

colMeans(data)
cov(data)

cov2cor(Sigma)
cor(data)

plot(data[, "X"], data[, "Y"],
     pch = 16, col = adjustcolor("#DF91A3FF", alpha.f = 0.4),
     xlab = "X", ylab = "Y",
     main = "Sebaran Data Normal Bivariat (n = 1000)")
abline(lm(Y ~ X, data = as.data.frame(data)), col = "red", lwd = 2)

cor.test(data[, "X"], data[, "Y"])