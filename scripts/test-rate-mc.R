library(ggplot2)
library(dplyr)
library(MSEtool)
library(EnvStats)
library(MSEtool)
library(MASS)
library(fitdistrplus)
library(skrunchy2025)

# Desired inputs: array of rates (exploitation). Maybe an option to manually give a CV to use?
# Like an option to use empirical CV or assign a CV?
# Desired outputs: array with same dimensions, with new resampled rates

library(devtools)
devtools::load_all(.)


d <- as.numeric(phi_dot_E[, 1])


# Multiplicative lognormal noise
# preserves 0 values

cv <- 0.3

# sample error from normal distribution, then transform with exp to get all positive values.

x_new <- d * exp(rnorm(length(d), 0, sqrt(log(1 + cv^2))))
plot(x_new)
points(d, col = "red")

log(1 / cv^2)
log(1 / 0.6^2)

sqrt(log(1 / cv^2))
sqrt(log(1 / 0.6^2))


plot(seq(-50, 100, 1))
exp(seq(-50, 100, 1))
plot(exp(seq(-50, 100, 1)))

plot(rnorm(length(d), 0, sqrt(log(1 + cv^2))))
plot(exp(rnorm(length(d), 0, sqrt(log(1 + cv^2)))))

plot(d ~ x_new)
abline(a = 0, b = 1)

mean(phi_dot_E == 0)
mean(phi_dot_M == 0)

which(phi_dot_E == 0)

which(phi_dot_M == 0)
phi_dot_M

plot(density(phi_dot_E[, 1]), xlim = c(0, 1))
lines(density(phi_dot_E[, 2]), col = "dodgerblue")
lines(density(phi_dot_E[, 3]), col = "firebrick")

# Use method of moments method to get starting values
x <- d[d > 0]

m <- mean(x)
v <- var(x)

tmp <- m * (1 - m) / v - 1

a0 <- m * tmp
b0 <- (1 - m) * tmp

c(a0, b0)


# Options:

# MASS:fitdistr()
fit <- MASS::fitdistr(
  d[d > 0],
  densfun = "beta",
  start = list(shape1 = a0, shape2 = b0)
)

fit

# fitdistrplus
fit <- fitdistrplus::fitdist(
  d[d > 0],
  distr = "beta",
  method = "mle",
  start = list(shape1 = a0, shape2 = b0)
)
fit
# results very similar

pi_hat


n <- length(d)
plot(d2 ~ d)
d
d2
hist(d)
hist(d2)

dput(d)

MASS::fitdistr(d, densfun = "beta", start = list(shape1 = 1, shape2 = 3))


fitdistr(d[-length(d)], densfun = "beta", start = list(shape1 = 1, shape2 = 3))

betaparam <- ebeta(phi_dot_E[, 1], method = "mle")
betaparam
str(betaparam)
hist(
  rbeta(
    100,
    shape1 = betaparam$parameters[1],
    shape2 = betaparam$parameters[2]
  ),
  breaks = seq(0, 1, 0.1)
)
hist(phi_dot_E[, 1], breaks = seq(0, 1, 0.1))

a <- alphaconv(m = mean(phi_dot_E[, 1]), sd = sd(phi_dot_E[, 1]))
b <- betaconv(m = mean(phi_dot_E[, 1]), sd(phi_dot_E[, 1]))
a
b

bt <- rbeta(n = 100, shape1 = a, shape2 = b)
hist(t)
hist(phi_dot_E)


est_beta_params <- function(mu, sigma, tol = 1e-8) {
  mu <- ifelse(mu == 0, mu + tol, mu) # if the value is 0, replace with very small number
  # Maximum allowable SD
  sigma_max <- sqrt(mu * (1 - mu)) # cap for sigma to get allowed shape parameters for beta distribution

  # Cap sigma slightly below the max (avoid zero/negative shapes)
  sigma <- pmin(sigma, sigma_max * (1 - tol))

  # Calculate variance from standard deviation
  var <- sigma^2

  # Method of moments to get shape parameters
  tmp <- (mu * (1 - mu)) / var - 1
  alpha <- mu * tmp
  beta <- (1 - mu) * tmp
  return(list(shape1 = alpha, shape2 = beta))
}

est_beta_params(mu = mean(phi_dot_E[, 1]), sigma = sd(phi_dot_E[, 1]))


# delta / hurdle model for zero-inflated beta distribution

pi_hat <- mean(d == 0)

u <- ifelse(
  runif(n) < pi_hat,
  0,
  rbeta(n, fit$estimate["shape1"], fit$estimate["shape2"])
)

r_beta_mixture <- function(n, mu, sigma, p0 = 0, p1 = 0, max_er = 0.5) {
  params <- est_beta_params(mu, sigma)

  u <- runif(n)

  x <- ifelse(
    u < p0,
    0,
    ifelse(u < p0 + p1, 1, rbeta(n, params$shape1, params$shape2))
  )

  x <- ifelse(x > max_er, 0.5, x)
  x
}


t1 <- r_beta_mixture(
  n = length(phi_dot_E[, 1]),
  mu = phi_dot_E[, 1],
  sigma = 0.05,
  p0 = 0,
  p1 = 0
)
t2 <- r_beta_mixture(
  n = length(phi_dot_E[, 2]),
  mu = phi_dot_E[, 2],
  sigma = 0.05,
  p0 = 0,
  p1 = 0
)
t3 <- r_beta_mixture(
  n = length(phi_dot_E[, 3]),
  mu = phi_dot_E[, 3],
  sigma = 0.05,
  p0 = 0,
  p1 = 0
)
#
ttog <- r_beta_mixture(
  n = length(phi_dot_E[]),
  mu = phi_dot_E[],
  sigma = sd(phi_dot_E[]),
  p0 = 0,
  p1 = 0
)

t <- c(t1, t2, t3)

t
plot(phi_dot_E, t)
abline(a = 0, b = 1)

points(phi_dot_E, ttog, col = "red", add = TRUE)

m1 <- lm(as.vector(phi_dot_E) ~ t)
m2 <- lm(as.vector(phi_dot_E) ~ ttog)

summary(m1)
summary(m2)


params <- est_beta_params(mu = 0, sigma = 0.07)

phi_dot_E

d <- array2DF(phi_dot_E)
ds <- d |>
  group_by(a) |>
  summarize(mean = mean(Value), SD = sd(Value), var = var(Value)) |>
  mutate(CV = SD / mean)

# Example usage:
print(params)

rbeta(n = 1, params$shape1, params$shape2)


# plot samples

df <- as.data.frame(phi_dot_E)

# convert to long format
df_long <- stack(df) # values + ind (column id)

ggplot(df_long, aes(x = values, colour = ind)) +
  geom_density(size = 1) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(x = "Value", y = "Density", colour = "Column") +
  theme_minimal()


# Generate random data

set.seed(123)
params <- est_beta_params(mean(phi_dot_E), sd(phi_dot_E))

x <- rbeta(1000, shape1 = params$shape1, params$shape2)

# Plot histogram of rnorm samples
hist(
  x,
  probability = TRUE, # scale to density
  col = "lightgray",
  border = "white",
  main = "rnorm() vs dnorm()",
  xlab = "Value"
)

# Add theoretical density curve
curve(dbeta(x, params$shape1, params$shape2), col = "red", lwd = 2, add = TRUE)
