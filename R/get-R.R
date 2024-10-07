#' Get recruits by brood year
#'
#' Calculate recruits by brood year, summing across ages.
#'
#' @param R_star_b Numeric, array of recruits by brood year, with three dimensions: population (i), brood year (b), and age (a).
#'
#' @return List with two elements. First element: numeric, array of recruits with two dimensions: population (i) and brood year (b).
#' Second element: data frame version of first element, for plotting and tables.
#'
#'
#' @examples
#'   populations <- c("Kitsumkalum", "Lower Skeena", "Zymoetz-Fiddler", "Upper Skeena", "Middle Skeena", "Large Lakes", "Skeena")
#'   n_populations <- length(populations)
#'   years <- 2000:2001
#'   n_years <- length(years)
#'   ages <- c(4,5,6,7)
#'   p_ages <- c(30,40,40,1)
#'   n_ages <- length(ages)
#'   MatureRun <- array(runif(min = 5000, max = 10000, n= n_populations*n_years*n_ages),
#'     dim = c(n_populations, n_years, n_ages), dimnames = list(i = populations, y = years, a = ages))
#'   MatureRun["Skeena",, ] <- apply(MatureRun[ !dimnames(MatureRun)$i =="Skeena",, ], c(2,3), sum)
#'   phi_Q <- array(runif( min = 0, max = 4000, n = length(MatureRun)), dim = dim(MatureRun), dimnames = dimnames(MatureRun))
#'   R_star <- get_R_star( MatureRun = MatureRun, phi_Q = phi_Q)
#'   R <- get_R(R_star_b = R_star$R_star_b)
#'
#' @export
get_R <- function(R_star_b, R_star_df) {
      R <- apply(R_star_b, c(1,2), sum, na.rm=TRUE)
      d <- aggregate( R_star ~ i + b + complete_brood, data = R_star_df, FUN = sum)
      names(d)[grep("R_star", names(d))] <- "R" # rename as R
      d$b <- as.integer(d$b)
      res <- list(R = R, df = d)
      res
}


