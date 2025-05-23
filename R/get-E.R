#' Get escapement by population
#'
#' Calculates E, escapement by population by subtracting freshwater terminal mortality from returns to Terrace if appropriate.
#'
#'
#' @param K Numeric, vector of estimates of Kitsumkalum Chinook spawners (ages 4, 5, 6, and 7, no jacks) based on a mark-recapture study and open population mark-recapture model (POPAN). One dimension: y (year). For more on POPAN models, see Cooch & White 2024 Chapter 12: http://www.phidot.org/software/mark/docs/book/pdf/chap12.pdf
#' @param X Numeric, array of returns to Terrace with two dimensions: population (i) and year (y).
#' @param Tau_U Integer, Chinook terminal mortalities from freshwater fisheries upsteam of Terrace, by year. Includes catch and incidental mortality estimate.
#' @param known_population Character, name of population i with known or estimated escapement. Defaults to Kitsumkalum.
#' @param aggregate_population Character, name of population i that is the sum of the other populations. Defaults to Skeena.
#' @param lower_populations Character vector, names of populations i downstream of Terrace. Defaults to Lower Skeena and Zymoetz-Fiddler.
#' @param upper_populations Character vector, names of populations i upstream of Terrace. Defaults to Upper Skeena, Middle Skeena, and Large Lakes.
#' @param save_csv If TRUE, save a csv of the data frame output.
#'
#' @return List with two elements. First element: numeric, matrix of escapement values for Chinook with two dimensions: population (i), and year (y).
#'         Second element: data frame version of matrix, for plotting and tables.
#'
#' @examples
#' E <- get_E(K = ex_k$kitsumkalum_escapement, X = ex_X, Tau_U = ex_Tau_U_total,
#'    known_population = "Kitsumkalum",
#'     aggregate_population = "Skeena",
#'     lower_populations = c("Lower Skeena", "Zymoetz-Fiddler"),
#'     upper_populations = c("Upper Skeena", "Middle Skeena", "Large Lakes"))
#'
#' @export
get_E <- function( K, X, Tau_U,
                   known_population = "Kitsumkalum",
                   aggregate_population = "Skeena",
                   lower_populations = c("Lower Skeena", "Zymoetz-Fiddler"),
                   upper_populations = c("Upper Skeena", "Middle Skeena", "Large Lakes"),
                   save_csv=FALSE) {

  # check that lengths (number of years) are equal in data inputs
  if(!isTRUE( identical(length(K), dim(X)[2]) && identical( length(K), length(Tau_U) )))  {
    stop("Length of year (y) dimensions not equal.") }

  populations <- dimnames(X)$i
  n_populations <- length(dimnames(X)$i)
  years <- dimnames(X)$y
  n_years <- length(dimnames(X)$y)
  # Add X returns to Terrace of upper populations together, by year
  X_U <- apply( X[ upper_populations, ], 2, sum )
  start_array <- array( data = rep(NA, times = length(X)), dim = dim(X),
                       dimnames = dimnames(X))
  E <- start_array
  for(y in 1:n_years) {
    # fill in Return to Terrace for Kitsumkalum with K, mark-recapture estimate
    E[known_population, y ] <- K[y]
    # Lower populations
    E[ lower_populations, y] <- X[lower_populations, y]
    # Upper populations
    for(i in upper_populations) {
      E[ i, y] <-   X[ i, y ] -  Tau_U[y] * X[ i, y ] / X_U[y]
    }
    # Skeena aggregate escapement
    E[aggregate_population, y] <- X[aggregate_population,y] - Tau_U[y]
  }

  d <- as.data.frame.table(E, responseName = "E", stringsAsFactors = FALSE)
  d$y <- as.integer(d$y)
  if(save_csv == TRUE) {
    write.csv(d, here("data-out/E.csv"), row.names = FALSE)
  }
  res <- list(E = E, df = d)
  res
}
