
#' Make fake genetic data and catch data
#'
#' Make three fake data arrays, one for genetic mixture analysis by week, and one weekly test fishery catch.
#'
#' @param catch_range Integer, length 2 vector of minimum and maximum catch values to sample between.
#' @param n_weeks Number of weeks fished.
#' @param n_years Number of years of data to create.
#' @param start_year Start year for fake data.
#' @param population_names Character vector with names of populatins.
#'
#' @return List with three elements. The first element is P, an array of genetic proportions with 3 dimensions: w (week), i (population), and y (year).
#' The second element is sigma_P, an array of SD of the genetic proportions, with 3 dimensions: w (week), i (population), and y (year).
#' The third element is G, an array of how many Chinook were caught in the gillnet Tyee Test Fishery by week, with 2 dimensions: w (week) and y (year).
#'
#' @examples
#' library(rrandvec)
#' library(abind)
#' res <- make_P_G()
#' P <- res$P
#' sigma_P <- res$sigma_P
#' G <- res$G
#'
#' @export
make_P_G <- function( catch_range = c(0,100), n_weeks = 12, n_years = 40, start_year = 1980,
                      population_names = c("Kitsumkalum", "Lower Skeena", "Middle Skeena", "Zymoetz-Fiddler", "Large Lakes", "Upper Skeena")) {
  catch_values <- seq( catch_range[1], catch_range[2], 1) # fake weekly catch values
  years <- seq(start_year, length.out=n_years) # years
  populations <- population_names # populations
  n_populations <- length(populations)

# Make array of genetic mixture proportions (add to 1)
P <- sapply(1:n_years, FUN = function(x) {
    rrandvec( n = n_weeks, d = n_populations)
  }, simplify = "array"
  )
P <- P * 100
# Make array of SD for genetic mixture proportions
sigma_P <- array( data = runif(n = n_weeks * n_years * n_populations, min=1,max=5),
                  dim = c( n_weeks, n_populations, n_years),
                  dimnames = list(w = 1:n_weeks, i = populations, y = years))

# make array of weekly catches
G <- array(sample(catch_values, n_weeks*n_years, replace=TRUE), dim=c(n_weeks, n_years),
        dimnames = list(w= 1:n_weeks, y= years))


# name array dimensions
  dimnames(P) <- list(w = 1:n_weeks, i = populations, y = years)
  res <- list(P = P, sigma_P = sigma_P, G = G)
  res
}
