#' Get spawners by population
#'
#' Calculates S, spawners by population by subtracting brood stock removals from Kitsumkalum and Skeena aggregate populations. All other populations, spawners = escapement.
#'
#' @param E numeric, matrix of escapement values for Chinook with two dimensions: population (i), and year (y). Chinook that 'escaped' fisheries en route to spawning river.
#' @param B Numeric, vector of number of brood stock adult Chinook taken from the Kitsumkalum river, by year (y).
#' @param brood_population Character, name of population i with where brood removals occurred. Defaults to Kitsumkalum.
#' @param aggregate_population Character, name of population i that is the sum of the other populations. Defaults to Skeena. Brood also need to removed from this since brood population is part of the aggregate.
#'
#' @return List with two elements. First element: numeric, matrix of escapement values for Chinook with two dimensions: population (i), and year (y).
#'         Second element: data frame version of matrix, for plotting and tables.
#'
#' @examples
#'   populations <- c("Kitsumkalum", "Lower Skeena", "Zymoetz-Fiddler", "Upper Skeena", "Middle Skeena", "Large Lakes", "Skeena")
#'   n_populations <- length(populations)
#'   years <- 2000:2001
#'   n_years <- length(years)
#'  E <- array(data = sample(2000:3000, size = n_populations* n_years, replace=TRUE), dim = c(n_populations, n_years),
#'                        dimnames = list( i =  populations, y = years))
#'  E["Skeena", ] <- apply(E[!dimnames(E)$i=="Skeena", ], 2, sum)
#'  B <- sample(50:100, size=n_years)
#'  S <- get_S( E = E, B = B)
#'
#' @export
get_S <- function( E, B,
                   brood_population = "Kitsumkalum",
                   aggregate_population = "Skeena") {
  populations <- dimnames(E)$i
  n_populations <- length(dimnames(E)$i)
  years <- dimnames(E)$y
  n_years <- length(dimnames(E)$y)
  no_brood_populations <- dimnames(E)$i[ !dimnames(E)$i %in% c(brood_population, aggregate_population)]
  start_array <- array(data = sample(c(999), size = n_populations* n_years, replace=TRUE), dim = c(n_populations, n_years),
                       dimnames = list( i =  populations, y = years))
  S <- start_array
  for(j in 1:n_years) {
    # Get Spawners for brood population and aggregate population. Escapement minus brood stock.
    S[brood_population, j ] <- E[ brood_population, j] - B[j]
    S[ aggregate_population, j] <- E[aggregate_population, j] - B[j]
    # All other populations, spawners = escapement (no brood)
    S[ no_brood_populations, j ] <- E[ no_brood_populations, j]
  }

  ds <- as.data.frame.table(S, responseName = "S")
  ds$y <- as.integer(as.character(ds$y))
  res <- list(S = S, df = ds)
  res
}
