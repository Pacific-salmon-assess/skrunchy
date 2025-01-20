#' Calculate Chinook returns to Terrace
#'
#' Calculate returns to Terrace of adult Chinook salmon (X) for population i and year y. Uses pooled genetic proportions and the number of Chinook returns to the Kitsumkalum River.
#'
#' @param P_tilde Numeric, array of genetic proportions at Skeena Tyee test fishery with 2 dimensions: i (population) and year (y).
#' @param sigma_P_tilde Numeric, array of SD of pooled genetic proportions with 2 dimensions: i (population) and y (year).
#' @param K Numeric, vector of estimates of Kitsumkalum Chinook spawners (ages 4, 5, 6, and 7, no jacks) based on a mark-recapture study and open population mark-recapture model (POPAN). One dimension: y (year). For more on POPAN models, see Cooch & White 2024 Chapter 12: http://www.phidot.org/software/mark/docs/book/pdf/chap12.pdf
#' @param sigma_K Numeric, vector of standard error (SE) of K. One dimension: y (year).
#' @param y_K Integer, vector of years for Kitsumkalum Chinook spawners K and sigma_K.
#' @param known_population Name of population i with known or estimated escapement. Defaults to Kitsumkalum.
#' @param aggregate_population Name of population i that is the sum of the other populations. Defaults to Skeena.
#' @param save_csv If TRUE, save a csv of the data frame output.
#'
#' @return A list with three elements. First element: Numeric, X which is an array of returns to Terrace with two dimensions: population (i) and year (y).
#' Second element: numeric, sigma_X which is an array of SE of returns to Terrace of Chinook, with two dimensions: population (i) and year (y).
#' Third element: dataframe with X, sigma_X, and year merged for plotting and reporting.
#' @examples
#' X <- get_X(P_tilde = ex_P_tilde, sigma_P_tilde = ex_sigma_P_tilde, K= ex_k$kitsumkalum_escapement,
#'             y_K = ex_k$year, sigma_K = ex_k$sd)
#'
#' @export
get_X <- function(P_tilde, sigma_P_tilde, K,  y_K, sigma_K, known_population = "Kitsumkalum",
                  aggregate_population = "Skeena", save_csv = FALSE) {
  # Add year, population checks
  # Year check - P_tilde, sigma_P_tilde, K, sigma_K, y
  if(!isTRUE( identical(dim(P_tilde)[2], dim(sigma_P_tilde)[2]) &&
              identical( dim(P_tilde)[2], length(K)) &&
              identical(  dim(P_tilde)[2], length(sigma_K)) &&
              identical(  dim(P_tilde)[2], length(y_K))))  {
    stop("Length of year (y) dimensions not equal.") }
  if(!all(dimnames(P_tilde)$y %in% dimnames(sigma_P_tilde)$y , dimnames(P_tilde)$y %in% unique(y_K) )) {
    stop("Year (y) values are not equal.") }
  # Population check - P_tilde, sigma_P_tilde
  if(!identical( dim(P_tilde)[1], dim(sigma_P_tilde)[1])) {
    stop("Length of population (i) dimensions not equal.") }
  if(!all(dimnames(P_tilde)$i %in% dimnames(sigma_P_tilde)$i) ) {
    stop("Population (i) values are not equal.")  }
  n_years <- length(dimnames(P_tilde)$y)
  n_populations <- length(dimnames(P_tilde)$i) + 1

  years <- dimnames(P_tilde)$y
  #populations <- dimnames(P_tilde)$i

  # make an array with bogus values to fill in with correct dimensions and names
  start_array <- array(data = rep(NA, n = n_populations* n_years), dim = c(n_populations, n_years),
                       dimnames = list( i = unlist(c( aggregate_population, as.vector(dimnames(P_tilde)["i"]))), y = dimnames(P_tilde)$y ))
  X <- start_array
  sigma_X <- start_array
  unknown_populations <- dimnames(X)$i[ !dimnames(X)$i %in% c(known_population, aggregate_population)]
  # X
  for(y in years) {
          # expand Kitsumkalum estimate by Kitsumkalum proportion to get aggregate
          X[aggregate_population, y] <- K[y_K == y] / P_tilde[known_population, y]
          # fill in Return to Terrace for Kitsumkalum with K, mark-recapture estimate
          X[known_population, y ] <- K[y_K == y]
          # Populations to fill in with proportion of aggregate
          # multiply proportion by aggregate to get estimate for populations without counts
          X[unknown_populations, y] <- X[aggregate_population,y] * P_tilde[unknown_populations, y]
  }
  # sigma_X
  for(y in years) {
    # sigma_X for return to Terrace, Skeena aggregate
     sigma_X[aggregate_population, y] <- get_sigma_X( X = X[aggregate_population, y],
                                                      K = K[y_K == y], sigma_K = sigma_K[ y_K == y],
                                                      P_tilde = P_tilde[known_population, y],
                                                      sigma_P_tilde = sigma_P_tilde[known_population, y],
                                                      aggregate_population = TRUE)
     sigma_X[known_population, y] <- sigma_K[ y_K == y] # fill in with SE for Kitsumkalum mark recapture estimate
     sigma_X[unknown_populations, y] <- NA # Leave as 0 for now, waiting to hear from Ivan
  }


  dt <- as.data.frame.table( X, responseName = "X", stringsAsFactors = FALSE)
  dst <- as.data.frame.table( sigma_X, responseName = "sigma_X", stringsAsFactors = FALSE)
  df_merged <- merge(dt, dst, by= c( "i", "y"))
  #df_merged <- d
  df_merged$y <- as.integer(  df_merged$y )
  if(save_csv == TRUE) {
    write.csv(df_merged, here("data-out/X.csv"), row.names = FALSE)
  }

  res <- list( X = X, sigma_X = sigma_X, df = df_merged )
  res
}
