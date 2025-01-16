#' Get wild spawners by age
#'
#' Calculates wild spawners, specific by age at return. Uses total wild spawners by year and population, and age proportions from Tyee.
#'
#'
#' @param S_star  Numeric, array of spawner values with three dimensions:  population (i), year (y), and age (a).
#' @param H_star Numeric, array of number of hatchery origin spawners returning to the population with a hatchery (e.g., Kitsumkalum River), by year (y) and age (a). Excludes age 3 fish (jacks).
#' @param hatchery_population Character, name of population i with where brood removals occurred. Defaults to Kitsumkalum.
#' @param aggregate_population Character, name of population i that is the sum of the other populations. Defaults to Skeena. Brood also need to removed from this since brood population is part of the aggregate.
#'
#' @return List with two elements. First element: numeric, array of wild spawner values with three dimensions:  population (i), year (y), and age (a).
#'          Second element: data frame version of first element in long format, for plotting and tables.
#'
#'
#' @examples
#'   W_star <- get_W_star(S_star = ex_S_star, H_star = ex_H_star,
#'     aggregate_population = "Skeena", hatchery_population = "Kitsumkalum")
#'
#' @export
get_W_star <- function(S_star, H_star, aggregate_population = "Skeena",
                       hatchery_population = "Kitsumkalum") {
    if(!dim(S_star)[3] == dim(H_star)[2] )  {
     stop("Length of age (a) dimensions not equal.") }
    if(!all(dimnames(S_star)$a %in% dimnames(H_star)$a )) {
      stop("Age (a) values are not equal.") }
    if(!dim(S_star)[2] == dim(H_star)[1] )  {
      stop("Length of year (y) dimensions not equal.") }
    if(!all(dimnames(S_star)$y %in% dimnames(H_star)$y )) {
      stop("Year (y) values are not equal.") }
    no_hatchery_populations <- dimnames(S_star)$i[ !dimnames(S_star)$i %in% c(hatchery_population, aggregate_population)]
    n_years <- dim(S_star)[2]
    n_ages <- dim(S_star)[3]
    # Flag: make year and age indexing explicit (to avoid mismatching)
    W_star <- array( data = NA, dim = dim(S_star), dimnames = dimnames(S_star))
      for(y in 1:n_years) {
        for(a in 1:n_ages){
          W_star[aggregate_population,y,a] <- S_star[aggregate_population,y,a] - H_star[y,a] # aggregate
          W_star[hatchery_population,y,a] <- S_star[hatchery_population,y,a] - H_star[y,a] # Kitsumkalum
          W_star[no_hatchery_populations ,y,a] <- S_star[no_hatchery_populations,y,a]   # all others
        }
      }
    d <- as.data.frame.table(W_star, responseName = "W_star", stringsAsFactors = FALSE)
    d$y <- as.integer(d$y)
    d$a <- as.integer(d$a)
    res <- list(W_star = W_star, df = d)
    res
}
