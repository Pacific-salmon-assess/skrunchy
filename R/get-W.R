#' Get wild spawners (natural origin) by population
#'
#' Calculates W, wild spawners by population by subtracting hatchery-origin spawners from Kitsumkalum and Skeena aggregate populations. All other populations, wild spawners = spawners.
#'
#' @param S  numeric, matrix of spawner values for Chinook with two dimensions: population (i), and year (y). Chinook that spawned (doesn't include brood stock removals).
#' @param H Numeric, vector of number of hatchery origin spawners returning to the population with a hatchery (e.g., Kitsumkalum River), by year (y).
#' @param hatchery_population Character, name of population i with where brood removals occurred. Defaults to Kitsumkalum.
#' @param aggregate_population Character, name of population i that is the sum of the other populations. Defaults to Skeena. Brood also need to removed from this since brood population is part of the aggregate.
#'
#' @return List with two elements. First element: numeric, array of wild spawner values for Chinook with two dimensions: population (i), and year (y).
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
#'  H <- sample(500:600, size=n_years)
#'  W <- get_W( S = S$S, H = H)
#' @export
get_W <- function( S, H,
                   hatchery_population = "Kitsumkalum",
                   aggregate_population = "Skeena") {
  populations <- dimnames(S)$i
  n_populations <- length(dimnames(S)$i)
  years <- dimnames(S)$y
  n_years <- length(dimnames(S)$y)
  no_hatchery_populations <- dimnames(S)$i[ !dimnames(S)$i %in% c(hatchery_population, aggregate_population)]
  start_array <- array(data = sample(c(999), size = n_populations* n_years, replace=TRUE), dim = c(n_populations, n_years),
                       dimnames = list( i =  populations, y = years))
  W <- start_array
  for(y in 1:n_years) {
    # Get Spawners for brood population and aggregate population. Escapement minus brood stock.
    W[hatchery_population, y ] <- S[ hatchery_population, y] - H[y]
    W[ aggregate_population, y] <- S[aggregate_population, y] - H[y]
    # All other populations, spawners = escapement (no brood)
    W[ no_hatchery_populations, y ] <- S[ no_hatchery_populations, y]
  }

  d <- as.data.frame.table(W, responseName = "W")
  d$y <- as.integer(as.character(d$y))
  res <- list(W = W, df = d)
  res
}
