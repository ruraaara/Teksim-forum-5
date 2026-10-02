set.seed(123)
n <- 1000
u <- runif(n)
xs <- ifelse(u < 0.6, rnorm(n, 0, 1), rnorm(n, 4, 1))
head(xs)

hist(xs, breaks = 30, freq = FALSE, col = "#FFB6C1",
     main = "Histogram Mixture 0.6N(0,1) + 0.4N(4,1)", xlab = "x")
curve(0.6 * dnorm(x, 0, 1) + 0.4 * dnorm(x, 4, 1),
      add = TRUE, col = "red", lwd = 2)

p_sim <- mean(xs > 2)
p_sim

p_teori <- 0.6 * pnorm(2, 0, 1, lower.tail = FALSE) +
  0.4 * pnorm(2, 4, 1, lower.tail = FALSE)
p_teori
abs(p_sim - p_teori)

#soal 2
set.seed(123)
n <- 5000
u1 <- runif(n)
u2 <- runif(n)
laju <- ifelse(u1 < 0.7, 1, 5)
xs2 <- -log(u2) / laju
head(xs2)
rata_sim <- mean(xs2)
p_sim <- mean(xs2 > 1)
rata_sim
p_sim

rata_teori <- 0.7 * 1 + 0.3 * (1 / 5)
p_teori <- 0.7 * exp(-1) + 0.3 * exp(-5)
c(rata_teori, p_teori)
c(abs(rata_sim - rata_teori), abs(p_sim - p_teori))

#soal 3

set.seed(123)
n <- 10000
u1 <- runif(n)
u2 <- runif(n)
xs3 <- ifelse(u1 < 0.4, 2 * u2, 3 + 2 * u2)
head(xs3)
data.frame(i = 1:10, u1, u2, xs3)
hist(xs3, breaks = 40, freq = FALSE, col = "#FFB6C1",
     main = "Histogram Mixture 0.4U(0,2) + 0.6U(3,5)", xlab = "x")
segments(0, 0.2, 2, 0.2, col = "red", lwd = 2)
segments(3, 0.3, 5, 0.3, col = "red", lwd = 2)

rata_sim <- mean(xs3)
rata_teori <- 0.4 * 1 + 0.6 * 4
c(rata_sim, rata_teori)
set.seed(123)
u <- runif(20)
u1 <- u[1:10]
u2 <- u[1:10]
u1
u2
#Soal 4
set.seed(123)
n <- 10000
u <- runif(n)
xs4 <- ifelse(u < 0.5, rgamma(n, shape = 3, rate = 1), rnorm(n, 5, 1))
head(xs4)
hist(xs4, breaks = 50, freq = FALSE, col = "#FFB6C1",
     main = "Histogram Mixture 0.5Gamma(3,1) + 0.5N(5,1)", xlab = "x")
curve(0.5 * dgamma(x, shape = 3, rate = 1) + 0.5 * dnorm(x, 5, 1),
      add = TRUE, col = "red", lwd = 2)
p_sim <- mean(xs4 < 2)
p_sim
rata_sim <- mean(xs4)
rata_teori <- 0.5 * 3 + 0.5 * 5
c(rata_sim, rata_teori)
p_teori <- 0.5 * pgamma(2, shape = 3, rate = 1) + 0.5 * pnorm(2, 5, 1)
p_teori
abs(p_sim - p_teori)
