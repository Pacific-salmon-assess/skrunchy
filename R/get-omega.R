#' Get age proportions
#'
#' Calculate age proportions (age at return) for populations, based on age data from Skeena River Tyee test fishery.
#'
#' @param n Numeric, array with three dimensions. Values are the number of fish of age a assigned to population i caught in return year y.
#'        First dimension: population (i), second dimension: year (y), third dimension: age (a).
#' @param save_csv If TRUE, save a csv of the data frame output.
#' @param save_location Sub-directory folder of where to save csv
#' @param save_name File name of csv to save. Defaults to "omega.csv"
#'
#' @return A list with two objects. First object: numeric, array of proportions of each age with three dimensions: population (i), year (y), and age (a).
#'        Second object: data frame with same data in long format (columns for population, year, and age) for plotting.
#'
#' @examples
#'   omega <- get_omega(ex_n)
#'
#'
#'
#' @export
get_omega <- function(n, save_csv = FALSE, save_location,
                      save_name = "omega.csv") {
  sum_n_year <- apply(n, c(1,2), sum) # sum number of aged fish by year.
  n_ages <- dim(n)[3]
  n_years <- dim(n)[2]
  # make array to fill in with age proportion values
  omega <- array( rep(99, length(n)), dim=dim(n), dimnames = dimnames(n))
  for(a in 1:n_ages) {
    for(y in 1:n_years) {
      # divide number of aged fish of each age by total aged in each year.
      omega[ ,y,a] <- n[ ,y,a] / sum_n_year[ ,y]
    }
  }
  # make data frame for plotting and tables
  d <- as.data.frame.table(omega, responseName = "omega", stringsAsFactors = FALSE)
  d$y <- as.integer(d$y)
  if(save_csv == TRUE) {
    save_to <- here(save_location, save_name )
    write.csv(d, save_to, row.names = FALSE)
  }
  res <- list( omega = omega, df = d )
  res
}
