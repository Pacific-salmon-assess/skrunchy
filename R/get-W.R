#' Get wild spawners (natural origin) by population
#'
#' Calculates wild spawners (W, not age specific) by population by either summing age-specific wild spawners (W_star) across ages, or subtracting hatchery-origin spawners (H) from spawners (S) for Kitsumkalum and Skeena aggregate populations.
#' All other populations, wild spawners (W) = spawners (S).
#' Not age-specific.
#'
#' @param method How to calcute spawners (W)? If "sum_ages", take the sum across ages of W_star (age-specific wild spawner array).
#'                If "subtract_hatchery", uses non-age-specific spawners (S) and subtracts non-age-specific hatchery spawners (H). Defaults to sum_ages.
#' @param W_star Numeric, array of wild spawner values with three dimensions:  population (i), year (y), and age (a).
#' @param S  numeric, matrix of spawner values for Chinook with two dimensions: population (i), and year (y). Chinook that spawned (doesn't include brood stock removals).
#' @param H Numeric, vector of number of hatchery origin spawners returning to the population with a hatchery (e.g., Kitsumkalum River), by year (y).
#' @param hatchery_population Character, name of population i with where brood removals occurred. Defaults to Kitsumkalum.
#' @param aggregate_population Character, name of population i that is the sum of the other populations. Defaults to Skeena. Brood also need to removed from this since brood population is part of the aggregate.
#'
#' @return List with two elements. First element: numeric, array of wild spawner values for Chinook with two dimensions: population (i), and year (y).
#'         Second element: data frame version of matrix, for plotting and tables.
#'
#' @examples
#' # Sum ages method
#' library(abind)
#' d <- make_P_G(start_year = 2000, end_year = 2001)
#' res <- get_P_tilde(P = d$P, sigma_P = d$sigma_P, G = d$G)
#' k <- data.frame( year = c(2000, 2001),
#'                 kitsumkalum_escapement = c(5000, 6000),
#'                 sd = c(500, 400))
#' X <- get_X(P_tilde = res$P_tilde, sigma_P_tilde = res$sigma_P_tilde, K= k$kitsumkalum_escapement,
#'           sigma_K = k$sd, y = k$year)
#' Tau_U <- sample(50:100, size = length(k$year), replace=TRUE)
#' E <- get_E(K = k$kitsumkalum_escapement, X = X$X, Tau_U = Tau_U,
#'    known_population = "Kitsumkalum",
#'     aggregate_population = "Skeena",
#'     lower_populations = c("Lower Skeena", "Zymoetz-Fiddler"),
#'     upper_populations = c("Upper Skeena", "Middle Skeena", "Large Lakes"))
#'   populations <- c("Kitsumkalum", "Lower Skeena", "Zymoetz-Fiddler", "Upper Skeena", "Middle Skeena", "Large Lakes", "Skeena")
#'   n_populations <- length(populations)
#'   years <- 2000:2001
#'   n_years <- length(years)
#'   ages <- c(4,5,6,7)
#'   p_ages <- c(30,40,40,1)
#'   n_ages <- length(ages)
#'   # Make up some age data
#'   d <- sapply(p_ages, FUN = function(x){ rpois( n = n_populations*n_years, lambda= x) })
#'   n <- array( d,  dim = c(n_populations, n_years, n_ages), dimnames = list(i = populations, y = years, a = ages))
#'   omega <- get_omega(n)
#'   K_star <- array(runif(n = n_years * n_ages, 5000, 20000), dim = c(n_years, n_ages), dimnames= list(y = years, a = ages))
#'   E_star <- get_E_star(E = E$E, omega = omega$omega, K_star = K_star)
#'   B_star <- array(sample(1:50, size= n_years*n_ages), dim = c(n_years, n_ages), dimnames = list( y = years, a = ages))
#'   B_star[,4] <- 0 # make age 7 brood = 0 so you don't get negative fish in S_star
#'   S_star <- get_S_star(E_star = E_star$E_star, B_star = B_star)
#'   H_star <- array(sample(10:40, size=n_years*n_ages), dim = c(n_years, n_ages), dimnames = list( y = years, a = ages))
#'   W_star <- get_W_star(S_star = S_star$S_star, H_star = H_star,
#'     aggregate_population = "Skeena", hatchery_population = "Kitsumkalum")
#'   W <- get_W(method = "sum_ages", W_star = W_star$W_star)
#'
#' # Subtract hatchery method
#'   populations <- c("Kitsumkalum", "Lower Skeena", "Zymoetz-Fiddler", "Upper Skeena", "Middle Skeena", "Large Lakes", "Skeena")
#'   n_populations <- length(populations)
#'   years <- 2000:2001
#'   n_years <- length(years)
#'  E <- array(data = sample(2000:3000, size = n_populations* n_years, replace=TRUE), dim = c(n_populations, n_years),
#'                        dimnames = list( i =  populations, y = years))
#'  E["Skeena", ] <- apply(E[!dimnames(E)$i=="Skeena", ], 2, sum)
#'  B <- sample(50:100, size=n_years)
#'  S <- get_S( method = "subtract_brood", E = E, B = B)
#'  H <- sample(500:600, size=n_years)
#'  W <- get_W( method = "subtract_hatchery", S = S$S, H = H)
#' @export
get_W <- function( method = "sum_ages",
                   W_star,
                   S, H,
                   hatchery_population = "Kitsumkalum",
                   aggregate_population = "Skeena") {
  if(method == "sum_ages") {
    W <- apply(W_star, c(1,2), sum, simplify = "array")
  }
  if(method == "subtract_hatchery") {
  populations <- dimnames(S)$i
  n_populations <- length(dimnames(S)$i)
  years <- dimnames(S)$y
  n_years <- length(dimnames(S)$y)
  no_hatchery_populations <- dimnames(S)$i[ !dimnames(S)$i %in% c(hatchery_population, aggregate_population)]
  start_array <- array(data = sample(c(999), size = n_populations* n_years, replace=TRUE), dim = c(n_populations, n_years),
                       dimnames = list( i =  populations, y = years))
  W <- start_array
  for(y in 1:n_years) {
    # Get Spawners for brood population and aggregate population. Escapement minus brood stock.
    W[hatchery_population, y ] <- S[ hatchery_population, y] - H[y]
    W[ aggregate_population, y] <- S[aggregate_population, y] - H[y]
    # All other populations, spawners = escapement (no brood)
    W[ no_hatchery_populations, y ] <- S[ no_hatchery_populations, y]
  }
  }
  d <- as.data.frame.table(W, responseName = "W")
  d$y <- as.integer(as.character(d$y))
  res <- list(W = W, df = d)
  res
}
