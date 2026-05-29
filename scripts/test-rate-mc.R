library(ggplot2)
library(dplyr)


# Desired inputs: array of rates (exploitation). Maybe an option to manually give a CV to use?
# Like an option to use empirical CV or assign a CV?
# Desired outputs: array with same dimensions, with new resampled rates

# Options:

sd(phi_dot_E)
mean(phi_dot_E)


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
# ttog <- r_beta_mixture(
#   n = length(phi_dot_E[]),
#   mu = phi_dot_E[],
#   sigma = sd(phi_dot_E[]),
#   p0 = 0,
#   p1 = 0
# )

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
