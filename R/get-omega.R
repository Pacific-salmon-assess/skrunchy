#' Get age proportions
#'
#' Calculate age proportions (age at return) for populations, based on age data from Skeena River Tyee test fishery.
#'
#' @param n Numeric, array with three dimensions. Values are the number of fish of age a assigned to population i caught in return year y.
#'        First dimension: population (i), second dimension: year (y), third dimension: age (a).
#'
#' @return A list with two objects. First object: numeric, array of proportions of each age with three dimensions: population (i), year (y), and age (a).
#'        Second object: data frame with same data in long format (columns for population, year, and age) for plotting.
#'
#' @examples
#'   populations <- c("Kitsumkalum", "Lower Skeena", "Zymoetz-Fiddler", "Upper Skeena", "Middle Skeena", "Large Lakes", "Skeena")
#'   n_populations <- length(populations)
#'   years <- 2000:2001
#'   n_years <- length(years)
#'   ages <- c(3,4,5,6,7)
#'   p_ages <- c(20,30,40,40,1)
#'   n_ages <- length(ages)
#'   # Make up some age data
#'   d <- sapply(p_ages, FUN = function(x){ rpois( n = n_populations*n_years, lambda= x) })
#'   n <- array( d,  dim = c(n_populations, n_years, n_ages), dimnames = list(i = populations, y = years, a = ages))
#'   omega <- get_omega(n)
#'
#'
#'
#' @export
get_omega <- function(n) {
  sum_n_year <- apply(n, c(1,2), sum)
  omega <- array( rep(99, length(n)), dim=c(n_populations, n_years, n_ages), dimnames = list(i = populations, y = years, a = ages))
  for(a in 1:n_ages) {
    for(y in 1:n_years) {
      omega[ ,y,a] <- n[ ,y,a] / sum_n_year[ ,y]
    }
  }
  omega
  d <- as.data.frame.table(omega, responseName = "omega")
  res <- list( omega = omega, df = d )
  res
}
