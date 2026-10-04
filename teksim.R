library(ggplot2)

rlomax <- function(n, alpha, lambda) {
  u <- runif(n)
  lambda * ((1 - u)^(-1/alpha) - 1)
}
plomax <- function(x, alpha, lambda) 1 - (1 + x/lambda)^(-alpha)
qlomax <- function(p, alpha, lambda) lambda * ((1 - p)^(-1/alpha) - 1)
dlomax <- function(x, alpha, lambda) (alpha/lambda) * (1 + x/lambda)^(-(alpha + 1))

rweibull2 <- function(n, k, lambda) {
  u <- runif(n)
  lambda * (-log(1 - u))^(1/k)
}

rrayleigh2 <- function(n, sigma) {
  u <- runif(n)
  sigma * sqrt(-2 * log(1 - u))
}

rarcsine2 <- function(n, a = 0, b = 1) {
  u <- runif(n)
  a + (b - a) * sin(pi * u / 2)^2
}
parcsine2 <- function(x, a = 0, b = 1) (2/pi) * asin(sqrt((x - a)/(b - a)))
qarcsine2 <- function(p, a = 0, b = 1) a + (b - a) * sin(pi * p / 2)^2
darcsine2 <- function(x, a = 0, b = 1) 1 / (pi * sqrt((x - a) * (b - x)))

rlogcauchy2 <- function(n, mu = 0, sigma = 1) {
  u <- runif(n)
  exp(mu + sigma * tan(pi * (u - 0.5)))
}

hist_plot <- function(x, dfun, title, ...) {
  ggplot(data.frame(x = x), aes(x = x)) +
    geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "steelblue", alpha = 0.6) +
    stat_function(fun = dfun, args = list(...), color = "red", linewidth = 1) +
    labs(title = title, x = "x", y = "Densitas") +
    theme_minimal()
}

qq_plot <- function(x, qfun, title, ...) {
  df <- data.frame(teoritis = qfun(ppoints(length(x)), ...), sampel = sort(x))
  ggplot(df, aes(teoritis, sampel)) +
    geom_point(color = "steelblue", alpha = 0.6) +
    geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1) +
    labs(title = title, x = "Kuantil teoritis", y = "Kuantil sampel") +
    theme_minimal()
}

set.seed(123)
n <- 2000

x1 <- rlomax(n, alpha = 3, lambda = 2)
x2 <- rweibull2(n, k = 2, lambda = 1)
x3 <- rrayleigh2(n, sigma = 1)
x4 <- rarcsine2(n)
x5 <- rlogcauchy2(n, mu = 0, sigma = 1)
y5 <- log(x5)

hist_plot(x1, dlomax, "Histogram Lomax(3,2)", alpha = 3, lambda = 2)
qq_plot(x1, qlomax, "QQ-plot Lomax(3,2)", alpha = 3, lambda = 2)

hist_plot(x2, dweibull, "Histogram Weibull(2,1)", shape = 2, scale = 1)
qq_plot(x2, qweibull, "QQ-plot Weibull(2,1)", shape = 2, scale = 1)

hist_plot(x3, dweibull, "Histogram Rayleigh(1)", shape = 2, scale = sqrt(2))
qq_plot(x3, qweibull, "QQ-plot Rayleigh(1)", shape = 2, scale = sqrt(2))

hist_plot(x4, darcsine2, "Histogram Arcsine(0,1)")
qq_plot(x4, qarcsine2, "QQ-plot Arcsine(0,1)")

hist_plot(y5, dcauchy, "Histogram ln(Log-Cauchy) ~ Cauchy(0,1)", location = 0, scale = 1)
qq_plot(y5, qcauchy, "QQ-plot Cauchy(0,1)", location = 0, scale = 1)

ks1 <- ks.test(x1, "plomax", alpha = 3, lambda = 2)
ks2 <- ks.test(x2, "pweibull", shape = 2, scale = 1)
ks3 <- ks.test(x3, "pweibull", shape = 2, scale = sqrt(2))
ks4 <- ks.test(x4, "parcsine2")
ks5 <- ks.test(y5, "pcauchy", location = 0, scale = 1)

hasil_ks <- data.frame(
  Distribusi = c("Lomax(3,2)", "Weibull(2,1)", "Rayleigh(1)", "Arcsine(0,1)", "Log-Cauchy(0,1)"),
  Statistik_D = c(ks1$statistic, ks2$statistic, ks3$statistic, ks4$statistic, ks5$statistic),
  P_value = c(ks1$p.value, ks2$p.value, ks3$p.value, ks4$p.value, ks5$p.value)
)

print(hasil_ks)