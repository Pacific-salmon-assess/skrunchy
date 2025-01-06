#' Get spawners by population, year, and age
#'
#' From escapement, calculate the number of spawners by population, year, and age by subtracting brood stock removals for appropriate populations.
#'
#' @param E_star Numeric, array of escapement values with three dimensions: population (i), year (y), and age (a).
#' @param B_star Numeric, array of number of brood stock adult Chinook taken from the Kitsumkalum river, by year (y) and age (a).
#' @param brood_population Character, name of population i with where brood removals occurred. Defaults to Kitsumkalum.
#' @param aggregate_population Character, name of population i that is the sum of the other populations. Defaults to Skeena. Brood also need to removed from this since brood population is part of the aggregate.
#' @param save_csv If TRUE, save a csv of the data frame output.
#'
#' @return List with two elements. First element: numeric, array of spawner values with three dimensions:  population (i), year (y), and age (a).
#'          Second element: data frame version of first element in long format, for plotting and tables.
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
#'   K_star <- array(runif(n = n_years * n_ages, 5000, 20000), dim = c(n_years, n_ages), dimnames= list(y = years, a = ages))
#'   E_star <- get_E_star(E = E$E, omega = omega$omega, K_star = K_star)
#'   B_star <- array(sample(1:50, size= n_years*n_ages), dim = c(n_years, n_ages), dimnames = list( y = years, a = ages))
#'   B_star[,4] <- 0 # make age 7 brood = 0 so you don't get negative fish in S_star
#'   S_star <- get_S_star(E_star = E_star$E_star, B_star = B_star)
#'
#'
#' @export
get_S_star <- function(E_star, B_star, aggregate_population = "Skeena",
                       brood_population = "Kitsumkalum",
                       save_csv = FALSE) {
  no_brood_populations <- dimnames(E_star)$i[ !dimnames(E_star)$i %in% c(brood_population, aggregate_population)]
  n_years <- dim(E_star)[2]
  n_ages <- dim(E_star)[3]
  S_star <- array( data = NA, dim = dim(E_star), dimnames = dimnames(E_star))
  for(y in 1:n_years) {
    for(a in 1:n_ages){
      S_star[aggregate_population,y,a] = E_star[aggregate_population,y,a] - B_star[y,a] # aggregate
      S_star[brood_population,y,a] = E_star[brood_population,y,a] - B_star[y,a] # Kitsumkalum
      S_star[no_brood_populations ,y,a] = E_star[no_brood_populations,y,a]   # all others
    }
  }
  d <- as.data.frame.table(S_star, responseName = "S_star", stringsAsFactors = FALSE)
  d$y <- as.integer(d$y)
  if(save_csv == TRUE) {
    write.csv(d, here("data-out/S_star.csv"), row.names = FALSE)
  }
  res <- list(S_star = S_star, df = d)
  res
}
