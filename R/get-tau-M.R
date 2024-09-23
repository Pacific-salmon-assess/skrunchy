#' Get total terminal mortality in marine areas
#'
#' Estimate the total terminal mortality (catch plus incidental mortality) in marine areas.
#' These include recreational and commercial catch near the Skeena River mouth, including Area 4.
#' This is based on Coded Wire Tag data and Chinook Technical Committee model output.
#'
#' @param W Numeric, array of wild spawner values for Chinook with two dimensions: population (i), year (y), and age (a). FLAG: need to create this.
#' @param tau_dot_M Terminal marine exploitation rate. Sum of exploitation rate for terminal
#' marine net fisheries and exploitation rate for terminal marine sport fisheries
#' (coded "TNBC TERM N" and "TNBC TERM S" by the Chinook Technical Committee, respectively), which are outputs from the Kitsumkalum CWT analysis.
#'
#' @return List with two elements. First element: numeric, array of terminal total mortalities in marine fisheries, with three dimensions: population (i), year (y), and age (a).
#' Second element: data frame version of first element for plotting and tables.
#'
#'
#' @examples
#'
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
#'  tau_dot_M <- sample(1:3, size=n_years)/10
#'  tau_M <- get_tau_M( W = W$W, tau_dot_M = tau_dot_M )
#'
#'
#' @export
get_tau_M <- function( W, tau_dot_M) {
  n_years <- dim(W)[2]
  # make blank array to fill in, with same dimensions and names as wild spawners array
  tau_M <- array(data = NA, dim = dim(W), dimnames = dimnames(W))
  for(y in 1:n_years) {
    tau_M[,,y] <- ( W[,,y] / ( 1 - tau_dot_M[,y]) ) - W[ ,,y]
  }
  d <- as.data.frame.table(tau_M, responseName = "tau_M")
  d$y <- as.integer(as.character(d$y))
  res <- list("tau_M" = tau_M, "df" = d)
  res
}
