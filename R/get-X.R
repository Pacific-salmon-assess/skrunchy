#' Calculate Chinook returns to Terrace
#'
#' Calculate returns to Terrace of adult Chinook salmon (X) for population i and year y. Uses pooled genetic proportions and the number of Chinook returns to the Kitsumkalum River.
#'
#' @param P_tilde Numeric, array of genetic proportions at Skeena Tyee test fishery with 2 dimensions: i (population) and year (y).
#' @param sigma_P_tilde Numeric, array of SD of pooled genetic proportions with 2 dimensions: i (population) and y (year).
#' @param K Numeric, vector of estimates of Kitsumkalum Chinook spawners (ages 4, 5, 6, and 7, no jacks) based on a mark-recapture study and open population mark-recapture model (POPAN). One dimension: y (year). For more on POPAN models, see Cooch & White 2024 Chapter 12: http://www.phidot.org/software/mark/docs/book/pdf/chap12.pdf
#' @param sigma_K Numeric, vector of standard error (SE) of K. One dimension: y (year).
#' @param y Integer, vector of years for Kitsumkalum Chinook spawners K and sigma_K.
#' @param known_population Name if population i with known or estimated escapement. Defaults to Kitsumkalum.
#' @param aggregate_population Name of population i that is the sum of the other populations. Defaults to Skeena.
#'
#' @return A list with three elements. First element: Numeric, X which is an array of returns to Terrace with two dimensions: population (i) and year (y).
#' Second element: numeric, sigma_X which is an array of SE of returns to Terrace of Chinook, with two dimensions: population (i) and year (y).
#' Third element: dataframe with X, sigma_X, and year merged for plotting and reporting.
#' @examples
#' library(abind)
#' d <- make_P_G(start_year = 2000, end_year = 2001)
#' res <- get_P_tilde(P = d$P, sigma_P = d$sigma_P, G = d$G)
#' k <- data.frame( year = c(2000, 2001),
#'                 kitsumkalum_escapement = c(5000, 6000),
#'                 sd = c(500, 400))
#' X <- get_X(P_tilde = res$P_tilde, sigma_P_tilde = res$sigma_P_tilde, K= k$kitsumkalum_escapement,
#'           sigma_K = k$sd, y = k$year)
#' X
#'
#' @export
get_X <- function(P_tilde, sigma_P_tilde, K, sigma_K, y, known_population = "Kitsumkalum",
                  aggregate_population = "Skeena") {
  stopifnot("Years do not match between data" = all.equal(unique(dimnames(P_tilde)$y),unique(dimnames(sigma_P_tilde)$y), unique(y)))
  n_years <- length(y)
  n_populations <- length(dimnames(P_tilde)$i) + 1
  # make an array with bogus values to fill in with correct dimensions and names
  start_array <- array(data = rep(NA, n = n_populations* n_years), dim = c(n_populations, n_years),
                       dimnames = list( i = unlist(c( aggregate_population, as.vector(dimnames(P_tilde)["i"]))), y = dimnames(P_tilde)$y ))
  X <- start_array
  sigma_X <- start_array
  unknown_populations <- dimnames(X)$i[ !dimnames(X)$i %in% c(known_population, aggregate_population)]
  # X
  for(y in 1:n_years) {
          # expand Kitsumkalum estimate by Kitsumkalum proportion to get aggregate
          X[aggregate_population, y] <- K[y] / P_tilde[known_population, y]
          # fill in Return to Terrace for Kitsumkalum with K, mark-recapture estimate
          X[known_population, y ] <- K[y]
          # Populations to fill in with proportion of aggregate
          # multiply proportion by aggregate to get estimate for populations without counts
          X[unknown_populations, y] <- X[aggregate_population,y] * P_tilde[unknown_populations, y]
  }
  # sigma_X
  for(y in 1:n_years) {
    # sigma_X for return to Terrace, Skeena aggregate
     sigma_X[aggregate_population, y] <- get_sigma_X( X = X[aggregate_population, y],
                                                      K = K[y], sigma_K = sigma_K[y],
                                                      P_tilde = P_tilde[known_population, y],
                                                      sigma_P_tilde = sigma_P_tilde[known_population, y],
                                                      aggregate_population = TRUE)
     sigma_X[known_population, y] <- sigma_K[y] # fill in with SE for Kitsumkalum mark recapture estimate
     sigma_X[unknown_populations, y] <- NA # Leave as 0 for now, waiting to hear from Ivan
  }


  dt <- as.data.frame.table( X, responseName = "X", stringsAsFactors = FALSE)
  dst <- as.data.frame.table( sigma_X, responseName = "sigma_X", stringsAsFactors = FALSE)
  df_merged <- merge(dt, dst, by= c( "i", "y"))
  #df_merged <- d
  df_merged$y <- as.integer(  df_merged$y )
  res <- list( X = X, sigma_X = sigma_X, df = df_merged )
  res
}
