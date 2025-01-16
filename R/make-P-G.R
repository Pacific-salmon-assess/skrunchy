
#' Make fake genetic data and catch data
#'
#' Make three fake data arrays: (1) proportions and (2) SD from genetic mixture analysis by population, week, and year,
#' and (3) test fishery Chinook catch by week and year.
#'
#' @param catch_range Integer, length 2 vector of minimum and maximum catch values to sample between.
#' @param n_weeks Number of weeks fished.
#' @param n_years Number of years of data to create. Default is NA
#' @param start_year Start year for fake data.
#' @param end_year End year for fake data.
#' @param population_names Character vector with names of populations.
#' @param seed Set seed to 1?
#'
#' @return List with three elements. The first element is P, an array of genetic proportions with 3 dimensions: i (population), w (week), and y (year).
#' The second element is sigma_P, an array of SD of the genetic proportions, with 3 dimensions: i (population), w (week),  and y (year).
#' The third element is G, an array of how many Chinook were caught in the gillnet Tyee Test Fishery by week, with 2 dimensions: w (week) and y (year).
#'
#' @examples
#' res <- make_P_G()
#' P <- res$P
#' sigma_P <- res$sigma_P
#' G <- res$G
#'
#' @export
make_P_G <- function( catch_range = c(0,100), n_weeks = 12, n_years = NA, start_year = 1984, end_year = 2023,
                      population_names = c("Kitsumkalum", "Lower Skeena", "Middle Skeena", "Zymoetz-Fiddler", "Large Lakes", "Upper Skeena"),
                      seed = TRUE) {
  if(seed == TRUE) set.seed(1)
  catch_values <- seq( catch_range[1], catch_range[2], 1) # fake weekly catch values
  if(is.na(n_years)) years <- seq(start_year, end_year, 1)
  if(!is.na(n_years)) years <- seq(start_year, length.out=n_years) # years
  populations <- population_names # populations
  n_populations <- length(populations)
  n_years <- length(years)

# Make array of genetic mixture proportions (add to 100)
# for each year, make a matrix of n_weeks random vectors of length n_populations, that add to 100.
# Then put the matrix for each year together into an array.
P <- sapply( 1:n_years, FUN = function(x) {
    fdat <- replicate( n_weeks, sample( 3:5, size = n_populations, replace=TRUE ))
    fdat1 <- apply( fdat, 2, FUN = function(x) { x / sum(x)} * 100)
    mdat <- matrix( data = fdat1, nrow = n_populations, ncol =  n_weeks, byrow=FALSE )
    mdat
  }, simplify = "array" # Use simplify = "array" keeps 3 dimensions. Making simplify = TRUE reduces it to 2 dimensions (removes week dimension)
)

# Make array of SD for genetic mixture proportions
sigma_P <- array( data = runif( n =  n_populations * n_weeks * n_years, min = 1, max = 5 ),
                  dim = c(  n_populations, n_weeks, n_years),
                  dimnames = list( i = populations, w = 1:n_weeks,  y = years ))

# make array of weekly catches
G <- array(sample(catch_values, n_weeks*n_years, replace=TRUE), dim=c(n_weeks, n_years),
        dimnames = list(w= 1:n_weeks, y= years))


# name array dimensions
  dimnames(P) <- list( i = populations, w = 1:n_weeks,  y = years )
  res <- list( P = P, sigma_P = sigma_P, G = G )
  res
}
