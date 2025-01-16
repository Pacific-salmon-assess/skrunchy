#' Get spawners by population, year, and age
#'
#' From escapement, calculate the number of spawners by population, year, and age by subtracting brood stock removals for appropriate populations.
#'
#' @param E_star Numeric, array of escapement values with three dimensions: population (i), year (y), and age (a).
#' @param B_star Numeric, array of number of brood stock adult Chinook taken from the Kitsumkalum river, by year (y) and age (a).
#' @param brood_population Character, name of population i with where brood removals occurred. Defaults to Kitsumkalum.
#' @param aggregate_population Character, name of population i that is the sum of the other populations. Defaults to Skeena. Brood also need to removed from this since brood population is part of the aggregate.
#' @param save_csv If TRUE, save a csv of the data frame output.
#' @param save_location Sub-directory folder of where to save csv
#' @param save_name File name of csv to save. Defaults to "S_star.csv"
#'
#' @return List with two elements. First element: numeric, array of spawner values with three dimensions:  population (i), year (y), and age (a).
#'          Second element: data frame version of first element in long format, for plotting and tables.
#'
#' @examples
#'   S_star <- get_S_star(E_star = ex_E_star, B_star = ex_B_star)
#'
#'
#' @export
get_S_star <- function(E_star, B_star, aggregate_population = "Skeena",
                       brood_population = "Kitsumkalum",
                       save_csv = FALSE, save_location,
                       save_name = "S_star.csv"
                       ) {
  # Age and year checks
  if(!dim(E_star)[3] == dim(B_star)[2] )  {
    stop("Length of age (a) dimensions not equal.") }
  if(!all(dimnames(E_star)$a %in% dimnames(B_star)$a )) {
    stop("Age (a) values are not equal.") }
  if(!dim(E_star)[2] == dim(B_star)[1] )  {
    stop("Length of year (y) dimensions not equal.") }
  if(!all(dimnames(E_star)$y %in% dimnames(B_star)$y )) {
    stop("Year (y) values are not equal.") }
  no_brood_populations <- dimnames(E_star)$i[ !dimnames(E_star)$i %in% c(brood_population, aggregate_population)]
  n_years <- dim(E_star)[2]
  n_ages <- dim(E_star)[3]
  S_star <- array( data = NA, dim = dim(E_star), dimnames = dimnames(E_star))
  for(y in 1:n_years) {
    for(a in 1:n_ages){
      S_star[aggregate_population,y,a] = E_star[aggregate_population,y,a] - B_star[y,a] # aggregate
      S_star[brood_population,y,a] = E_star[brood_population,y,a] - B_star[y,a] # Kitsumkalum
      S_star[no_brood_populations ,y,a] = E_star[no_brood_populations,y,a]   # all others
    }
  }
  d <- as.data.frame.table(S_star, responseName = "S_star", stringsAsFactors = FALSE)
  d$y <- as.integer(d$y)
  d$a <- as.integer(d$a)
  if(save_csv == TRUE) {
    save_to <- here(save_location, save_name )
    write.csv(d, save_to, row.names = FALSE)
  }
  res <- list(S_star = S_star, df = d)
  res
}
