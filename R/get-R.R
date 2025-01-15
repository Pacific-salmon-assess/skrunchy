#' Get recruits by brood year
#'
#' Calculate recruits by brood year, summing across ages.
#'
#' @param R_star_b Numeric, array of recruits by brood year, with three dimensions: population (i), brood year (b), and age (a).
#' @param R_star_df Numeric, data frame of recruits by poopulation, return year, brood year, and age.
#'
#' @return List with two elements. First element: numeric, array of recruits with two dimensions: population (i) and brood year (b).
#' Second element: data frame version of first element, for plotting and tables.
#'
#'
#' @examples
#'   R <- get_R(R_star_b = ex_R_star$R_star_b, R_star_df = ex_R_star$df)
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


