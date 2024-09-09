#' Calculate Chinook returns to Terrace
#'
#' Calculate returns to Terrace of adult Chinook salmon for population i and year y. Uses pooled genetic proportions and the number of Chinook returns to the Kitsumkalum River.
#'
#' @param P_tilde Numeric, array of genetic proportions at Skeena Tyee test fishery with 2 dimensions: i (population) and year (y).
#' @param sigma_P_tilde Numeric, array of SD of pooled genetic proportions with 2 dimensions: i (population) and y (year).
#' @param K Numeric, vector of estimates of Kitsumkalum Chinook spawners (ages 4, 5, 6, and 7, no jacks) based on a mark-recapture study and open population mark-recapture model (POPAN). One dimension: y (year). For more on POPAN models, see Cooch & White 2024 Chapter 12: http://www.phidot.org/software/mark/docs/book/pdf/chap12.pdf
#' @param sigma_K Numeric, vector of standard error (SE) of K. One dimension: y (year).
#' @param y Integer, vector of years for Kitsumkalum Chinook spawners K and sigma_K.
#' @param known_population Population i with known or estimated escapement. Defaults to Kitsumkalum.
#' @param aggregate_population Population i that is the sum of the other populations. Defaults to Skeena.
#'
#' @return A list with three elements. First element: Numeric, T which is an array of returns to Terrace with two dimensions: population (i) and year (y).
#' Second element: numeric, sigma_T which is an array of SE of returns to Terrace of Chinook, with two dimensions: population (i) and year (y).
#' Third element: dataframe with T, sigma_T, and year merged for plotting and reporting.
#' @examples
#' library(abind)
#' d <- make_P_G(start_year = 2000, end_year = 2001)
#' res <- get_P_tilde(P = d$P, sigma_P = d$sigma_P, G = d$G)
#' k <- data.frame( year = c(2000, 2001),
#'                 kitsumkalum_escapement = c(5000, 6000),
#'                 sd = c(500, 400))
#' T <- get_T(P_tilde = res$P_tilde, sigma_P_tilde = res$sigma_P_tilde, K= k$kitsumkalum_escapement,
#'           sigma_K = k$sd, y = k$year)
#' T
#'
#' @export
get_T <- function(P_tilde, sigma_P_tilde, K, sigma_K, y, known_population = "Kitsumkalum",
                  aggregate_population = "Skeena") {
  stopifnot("Years do not match between data" = all.equal(unique(dimnames(P_tilde)$y),unique(dimnames(sigma_P_tilde)$y), unique(y)))
  n_years <- length(y)
  n_populations <- length(dimnames(P_tilde)$i) + 1
  # make an array with bogus values to fill in with correct dimensions and names
  start_array <- array(data = seq(1, length.out = n_populations* n_years), dim = c(n_populations, n_years),
                       dimnames = list( i = unlist(c( aggregate_population, as.vector(dimnames(P_tilde)["i"]))), y = dimnames(P_tilde)$y ))
  T <- start_array
  sigma_T <- start_array
  unknown_populations <- dimnames(T)$i[ !dimnames(T)$i %in% c(known_population, aggregate_population)]
  for(j in 1:n_years) {
          # expand Kitsumkalum estimate by Kitsumkalum proportion to get aggregate
          T[aggregate_population, j] <- K[j] / P_tilde[known_population, j]
          # fill in Return to Terrace for Kitsumkalum with K, mark-recapture estimate
          T[known_population, j ] <- K[j]
          # Populations to fill in with proportion of aggregate
          # multiply proportion by aggregate to get estimate for populations without counts
          T[unknown_populations, j] <- T[aggregate_population,j] * P_tilde[unknown_populations, j]
  }
  # sigma_T
  for(j in 1:n_years) {
    # sigma_T for return to Terrace, Skeena aggregate
     sigma_T[aggregate_population, j] <- get_sigma_T( T = T[aggregate_population, j],
                                                      K = K[j], sigma_K = sigma_K[j],
                                                      P_tilde = P_tilde[known_population, j],
                                                      sigma_P_tilde = sigma_P_tilde[known_population, j],
                                                      aggregate_population = TRUE)
     sigma_T[known_population, j] <- sigma_K[j] # fill in with SE for Kitsumkalum mark recapture estimate
     sigma_T[unknown_populations, j] <- NA # Leave as 0 for now, waiting to hear from Ivan
  }


  dt <- as.data.frame.table( T, responseName = "T")
  dst <- as.data.frame.table( sigma_T, responseName = "sigma_T")
  df_merged <- merge(dt, dst, by= c( "i", "y"))
  #df_merged <- d
  df_merged$y <- as.integer( as.character( df_merged$y ))
  res <- list( T = T, sigma_T = sigma_T, df_merged = df_merged )
}
