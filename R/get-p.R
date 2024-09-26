#' Get proportion wild spawners
#'
#' Calculate the proportion of total spawners that are wild spawners (natural origin).
#'
#' @param W_star Numeric, array of wild spawner values with three dimensions:  population (i), year (y), and age (a).
#' @param E_star Numeric, array of escapement values with three dimensions: population (i), year (y), and age (a).
#' @param hatchery_population Character, name of population i with where brood removals occurred. Defaults to Kitsumkalum.
#' @param aggregate_population Character, name of population i that is the sum of the other populations. Defaults to Skeena. Brood also need to removed from this since brood population is part of the aggregate.
#'
#'
#' @return A list with two elements. First element: numeric, an array of wild spawner proportions with values between 0 and 1, with three dimensions: population (i), year (y), and age (a).
#'          Second element: data frame version of matrix, for plotting and tables.
#' @examples
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
#'   E_star <- get_E_star(E = E$E, omega = omega$omega)
#'   B_star <- array(sample(1:50, size= n_years*n_ages), dim = c(n_years, n_ages), dimnames = list( y = years, a = ages))
#'   B_star[,4] <- 0 # make age 7 brood = 0 so you don't get negative fish in S_star
#'   S_star <- get_S_star(E_star = E_star$E_star, B_star = B_star)
#'   H_star <- array(sample(10:40, size=n_years*n_ages), dim = c(n_years, n_ages), dimnames = list( y = years, a = ages))
#'   H_star[,4] <- 0 # make age 7 hatchery = 0 so you don't get negative fish in W_star
#'   W_star <- get_W_star(S_star = S_star$S_star, H_star = H_star,
#'     aggregate_population = "Skeena", hatchery_population = "Kitsumkalum")
#'  p <- get_p(W_star = W_star$W_star, E_star = E_star$E_star)
#'
#' @export
get_p <- function( W_star, E_star, hatchery_population = "Kitsumkalum",
                   aggregate_population = "Skeena") {
  n_years <- dim(W_star)[2]
  n_ages <- dim(W_star)[3]
  no_hatchery_populations <- dimnames(W_star)$i[ !dimnames(W_star)$i %in% c(hatchery_population, aggregate_population)]
  p <- array(data = NA, dim = dim(W_star), dimnames = dimnames(W_star))
  for(y in 1:n_years) {
    for(a in 1:n_ages) {
    # Get proportion wild spawners for hatchery population and aggregate population. Wild spawners divided by escapement. FLAG: why isn't this divided by total spawners?
    p[hatchery_population, y, a ] <- W_star[ hatchery_population, y, a]  / E_star[hatchery_population, y, a]
    p[ aggregate_population, y, a ] <- W_star[ aggregate_population, y, a]  / E_star[aggregate_population, y, a]
    # All other populations, p = 1 (no hatchery origin spawners)
    p[ no_hatchery_populations, y ,a ] <- 1
    }
  }
  p1 <- p
  p1[ is.nan(p1)] <- 1 # replace NaN from 0/0 with 1. This is for cases where there were 0 spawners and 0 wild spawners.
  d <- as.data.frame.table(p1, responseName = "p", stringsAsFactors = FALSE)
  d$y <- as.integer(d$y)
  res <- list(p = p1, df = d)
  res
}
