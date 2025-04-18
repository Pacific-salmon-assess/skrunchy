#' Get proportion wild spawners
#'
#' Calculate the proportion of total escapement that are wild (natural origin).
#'
#' @param W_star Numeric, array of wild spawner values with three dimensions:  population (i), year (y), and age (a).
#' @param E_star Numeric, array of escapement values with three dimensions: population (i), year (y), and age (a).
#' @param B_star Numeric, array of brood stock removals with three dimensions: population (i), year (y), and age (a).
#' @param hatchery_population Character, name of population i with where brood removals occurred. Defaults to Kitsumkalum.
#' @param aggregate_population Character, name of population i that is the sum of the other populations. Defaults to Skeena. Brood also need to removed from this since brood population is part of the aggregate.
#'
#'
#' @return A list with two elements. First element: numeric, an array of wild spawner proportions with values between 0 and 1, with three dimensions: population (i), year (y), and age (a).
#'          Second element: data frame version of matrix, for plotting and tables.
#' @examples
#'  p <- get_p(W_star = ex_W_star, E_star = ex_E_star, B_star = ex_B_star)
#'
#' @export
get_p <- function( W_star, E_star, B_star, hatchery_population = "Kitsumkalum",
                   aggregate_population = "Skeena") {
  # age and year checks
  if(!dim(W_star)[3] == dim(E_star)[3] )  {
    stop("Length of age (a) dimensions not equal.") }
  if(!all(dimnames(W_star)$a %in% dimnames(E_star)$a )) {
    stop("Age (a) values are not equal.") }
  if(!dim(W_star)[2] == dim(E_star)[2] ) {
    stop("Length of year (y) dimensions not equal.") }
  if(!all(dimnames(W_star)$y %in% dimnames(E_star)$y )) {
    stop("Year (y) values are not equal.")    }
  # Check B_star dimensions
  if(!dim(W_star)[3] == dim(B_star)[2] )  {
    stop("Length of age (a) dimensions not equal.") }
  if(!all(dimnames(W_star)$a %in% dimnames(B_star)$a )) {
    stop("Age (a) values are not equal.") }
  if(!dim(W_star)[2] == dim(B_star)[1] ) {
    stop("Length of year (y) dimensions not equal.") }
  if(!all(dimnames(W_star)$y %in% dimnames(B_star)$y )) {
    stop("Year (y) values are not equal.")    }

  n_years <- dim(W_star)[2]
  n_ages <- dim(W_star)[3]
  no_hatchery_populations <- dimnames(W_star)$i[ !dimnames(W_star)$i %in% c(hatchery_population, aggregate_population)]
  p <- array(data = NA, dim = dim(W_star), dimnames = dimnames(W_star))
  for(y in 1:n_years) {
    for(a in 1:n_ages) {
    # Get proportion wild spawners for hatchery population and aggregate population. Wild spawners + brood divided by escapement.
    p[hatchery_population, y, a ] <- ( W_star[ hatchery_population, y, a] +  B_star[ y, a])  / E_star[hatchery_population, y, a]
    p[ aggregate_population, y, a ] <- ( W_star[ aggregate_population, y, a] + B_star[ y, a] ) / E_star[aggregate_population, y, a]
    # All other populations, p = 1 (no hatchery origin spawners)
    p[ no_hatchery_populations, y ,a ] <- 1
    }
  }
  p1 <- p
  p1[ is.nan(p1)] <- 1 # replace NaN from 0/0 with 1. This is for cases where there were 0 spawners and 0 wild spawners.
  d <- as.data.frame.table(p1, responseName = "p", stringsAsFactors = FALSE)
  d$y <- as.integer(d$y)
  d$a <- as.integer(d$a)
  res <- list(p = p1, df = d)
  res
}
