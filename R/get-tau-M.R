#' Get total terminal mortality in marine areas
#'
#' Estimate the total terminal mortality (catch plus incidental mortality) in marine areas.
#' These include recreational and commercial catch near the Skeena River mouth, including Area 4.
#' This is based on Coded Wire Tag data and Chinook Technical Committee model output.
#'
#' @param W_star Numeric, array of wild spawner values for Chinook with two dimensions: population (i), year (y), and age (a).
#' @param tau_dot_M Numeric, array of terminal marine exploitation rates with dimensions year (y) and age (a). Sum of exploitation rate for terminal
#' marine net fisheries and exploitation rate for terminal marine sport fisheries
#' (coded "TNBC TERM N" and "TNBC TERM S" by the Chinook Technical Committee, respectively), which are outputs from the Kitsumkalum CWT analysis.
#'
#' @return List with two elements. First element: numeric, array of terminal total mortalities in marine fisheries, with three dimensions: population (i), year (y), and age (a).
#' Second element: data frame version of first element for plotting and tables.
#'
#'
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
#'   S_star <- get_S_star(E_star = E_star$E_star, B_star = B_star)
#'   H_star <- array(sample(10:40, size=n_years*n_ages), dim = c(n_years, n_ages), dimnames = list( y = years, a = ages))
#'   W_star <- get_W_star(S_star = S_star$S_star, H_star = H_star,
#'     aggregate_population = "Skeena", hatchery_population = "Kitsumkalum")
#'  tau_dot_M <- array( data = sample(1:3, size=n_years*n_ages, replace =TRUE)/10, dim = c(n_years, n_ages),dimnames = list("y" = years, "a" = ages))
#'  tau_M <- get_tau_M( W_star = W_star$W_star, tau_dot_M = tau_dot_M )
#'
#'
#' @export
get_tau_M <- function( W_star, tau_dot_M) {
  populations <- dimnames(W_star)$i
  n_years <- dim(W_star)[2]
  n_ages <- dim(W_star)[3]
  # make blank array to fill in, with same dimensions and names as wild spawners array
  tau_M <- array(data = NA, dim = dim(W_star), dimnames = dimnames(W_star))
  for(i in populations) {
    for(y in 1:n_years) {
      for(a in 1:n_ages) {
      tau_M[i,y,a] <- ( W_star[i,y,a] / ( 1 - tau_dot_M[y,a]) ) - W_star[i,y,a]
      }
    }
  }
  d <- as.data.frame.table(tau_M, responseName = "tau_M")
  d$y <- as.integer(as.character(d$y))
  res <- list("tau_M" = tau_M, "df" = d)
  res
}
