#' Get escapement by population
#'
#' Calculates E, escapement by population by subtracting freshwater terminal mortality from returns to Terrace if appropriate.
#'
#' @param K Numeric, vector of estimates of Kitsumkalum Chinook spawners (ages 4, 5, 6, and 7, no jacks) based on a mark-recapture study and open population mark-recapture model (POPAN). One dimension: y (year). For more on POPAN models, see Cooch & White 2024 Chapter 12: http://www.phidot.org/software/mark/docs/book/pdf/chap12.pdf
#' @param X Numeric, array of returns to Terrace with two dimensions: population (i) and year (y).
#' @param tau_F_U Integer, Chinook terminal mortalities from freshwater fisheries upsteam of Terrace. Includes catch and incidental mortality estimate.
#' @param known_population Character, name of population i with known or estimated escapement. Defaults to Kitsumkalum.
#' @param aggregate_population Character, name of population i that is the sum of the other populations. Defaults to Skeena.
#' @param lower_populations Character vector, names of populations i downstream of Terrace. Defaults to Lower Skeena and Zymoetz-Fiddler.
#' @param upper_populations Character vector, names of populations i upstream of Terrace. Defaults to Upper Skeena, Middle Skeena, and Large Lakes.
#'
#' @return List with two elements. First element: numeric, matrix of escapement values for Chinook with two dimensions: population (i), and year (y).
#'         Second element: data frame version of matrix, for plotting.
#'
#' @examples
#' library(abind)
#' library(here)
#' d <- make_P_G(start_year = 2000, end_year = 2001)
#' res <- get_P_tilde(P = d$P, sigma_P = d$sigma_P, G = d$G)
#' k <- data.frame( year = c(2000, 2001),
#'                 kitsumkalum_escapement = c(5000, 6000),
#'                 sd = c(500, 400))
#' X <- get_X(P_tilde = res$P_tilde, sigma_P_tilde = res$sigma_P_tilde, K= k$kitsumkalum_escapement,
#'           sigma_K = k$sd, y = k$year)
#' tau_F_U <- sample(50:100, n = length(k$year), replace=TRUE)
#' E <- get_E(K = k$kitsumkalum_escapement, X = X$X, tau_F_U = tau_F_U,
#'    known_population = "Kitsumkalum",
#     aggregate_population = "Skeena",
#     lower_populations = c("Lower Skeena", "Zymoetz-Fiddler"),
#     upper_populations = c("Upper Skeena", "Middle Skeena", "Large Lakes"))
#'
#' @export
get_E <- function( K, X, tau_F_U,
                   known_population = "Kitsumkalum",
                   aggregate_population = "Skeena",
                   lower_populations = c("Lower Skeena", "Zymoetz-Fiddler"),
                   upper_populations = c("Upper Skeena", "Middle Skeena", "Large Lakes")) {
  populations <- dimnames(X)$i
  n_populations <- length(dimnames(X)$i)
  years <- dimnames(X)$y
  n_years <- length(dimnames(X)$y)
  # Add X returns to Terrace of upper populations together, by year
  X_U <- apply( X[ upper_populations, ], 2, sum )
  start_array <- array(data = sample(c(777,888,999), size = n_populations* n_years, replace=TRUE), dim = c(n_populations, n_years),
                       dimnames = list( i =  populations, y = years))
  E <- start_array
  for(j in 1:n_years) {
    # fill in Return to Terrace for Kitsumkalum with K, mark-recapture estimate
    E[known_population, j ] <- K[j]
    # Lower populations
    E[ lower_populations, j] <- X[lower_populations, j]
    # Upper populations
    for(k in 1:length(upper_populations)) {
      E[ upper_populations[k], j ] <-   X[ upper_populations[k], j ] -  tau_F_U[j] * X[ upper_populations[k], j ] / X_U[j]
    }
    # Skeena aggregate escapement
    E[aggregate_population, j] <- X[aggregate_population,j] - tau_F_U[j]
  }

  de <- as.data.frame.table(E, responseName = "E")
  de$y <- as.integer(as.character(de$y))
  res <- list(E = E, df = de)
  res
}
