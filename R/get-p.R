#' Get proportion wild spawners
#'
#' Calculate the proportion of total spawners that are wild spawners (natural origin).
#'
#' @param W Numeric, matrix of wild spanwer values for Chinook with two dimensions: population (i), and year (y).
#' @param E Numeric, matrix of escapement values for Chinook with two dimensions: population (i), and year (y). Chinook that 'escaped' fisheries en route to spawning river.
#' @param hatchery_population Character, name of population i with where brood removals occurred. Defaults to Kitsumkalum.
#' @param aggregate_population Character, name of population i that is the sum of the other populations. Defaults to Skeena. Brood also need to removed from this since brood population is part of the aggregate.
#'
#'
#' @return A list with two elements. First element: numeric, a matrix of wild spawner proportions with values between 0 and 1, with two dimensions: population (i) and year (y).
#'          Second element: data frame version of matrix, for plotting and tables.
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
#'  p <- get_p( W = W$W, E = E)
#'
#' @export
get_p <- function( W, E, hatchery_population = "Kitsumkalum",
                   aggregate_population = "Skeena") {
  populations <- dimnames(W)$i
  n_populations <- length(dimnames(W)$i)
  years <- dimnames(W)$y
  n_years <- length(dimnames(W)$y)
  no_hatchery_populations <- dimnames(W)$i[ !dimnames(W)$i %in% c(hatchery_population, aggregate_population)]
  start_array <- array(data = sample(c(999), size = n_populations* n_years, replace=TRUE), dim = c(n_populations, n_years),
                       dimnames = list( i =  populations, y = years))
  p <- start_array
  for(y in 1:n_years) {
    # Get proportion wild spawners for hatchery population and aggregate population. Wild spawners divided by escapement. FLAG: why isn't this divided by total spawners?
    p[hatchery_population, y ] <- W[ hatchery_population, y]  / E[hatchery_population, y]
    p[ aggregate_population, y] <- W[ aggregate_population, y]  / E[aggregate_population, y]
    # All other populations, p = 1 (no hatchery origin spawners)
    p[ no_hatchery_populations, y ] <- 1
  }

  d <- as.data.frame.table(p, responseName = "p")
  d$y <- as.integer(as.character(d$y))
  res <- list(p = p, df = d)
  res
}
