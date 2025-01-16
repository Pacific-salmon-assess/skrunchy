#' Get total terminal mortality in marine areas
#'
#' Estimate the total terminal mortality (catch plus incidental mortality) in marine areas.
#' These include recreational and commercial catch near the Skeena River mouth, including Area 4.
#' This is based on Coded Wire Tag data and Chinook Technical Committee model output.
#'
#' @param W_star Numeric, array of wild spawner values for Chinook with two dimensions: population (i), year (y), and age (a).
#' @param tau_dot_M Numeric, array of terminal marine exploitation rates with dimensions year (y) and age (a). Sum of exploitation rate for terminal
#' marine net fisheries and exploitation rate for terminal marine sport fisheries
#' (coded "TNBC TERM N" and "TNBC TERM S" by the Chinook Technical Committee, respectively), which are outputs from the Kitsumkalum CWT analysis.
#'
#' @return List with two elements. First element: numeric, array of terminal total mortalities in marine fisheries, with three dimensions: population (i), year (y), and age (a).
#' Second element: data frame version of first element for plotting and tables.
#'
#'
#' @examples
#'  tau_M <- get_tau_M( W_star = ex_W_star, tau_dot_M = ex_tau_dot_M )
#'
#'
#' @export
get_tau_M <- function( W_star, tau_dot_M) {
  # Check year and age
  # Ages
  if(!dim(W_star)[3] == dim(tau_dot_M)[2] )  {
    stop("Length of age (a) dimensions not equal.") }
  if(!all(dimnames(W_star)$a %in% dimnames(tau_dot_M)$a )) {
    stop("Age (a) values are not equal.") }
  # Years
  if(!dim(W_star)[2] == dim(tau_dot_M)[1] )  {
    stop("Length of year (y) dimensions not equal.") }
  if(!all(dimnames(W_star)$y %in% dimnames(tau_dot_M)$y )) {
    stop("Year (y) values are not equal.") }

  populations <- dimnames(W_star)$i
  n_years <- dim(W_star)[2]
  n_ages <- dim(W_star)[3]
  # make blank array to fill in, with same dimensions and names as wild spawners array
  tau_M <- array(data = NA, dim = dim(W_star), dimnames = dimnames(W_star))
  for(i in populations) {
    for(y in 1:n_years) {
      for(a in 1:n_ages) {
      tau_M[i,y,a] <- ( W_star[i,y,a] / ( 1 - tau_dot_M[y,a]) ) - W_star[i,y,a]
      }
    }
  }
  d <- as.data.frame.table(tau_M, responseName = "tau_M")
  d$y <- as.integer(as.character(d$y))
  d$a <- as.integer(d$a)
  res <- list("tau_M" = tau_M, "df" = d)
  res
}
