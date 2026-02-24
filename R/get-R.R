#' Get recruits brood year
#'
#' Calculate recruits by return year and brood year by adding up total run by brood year.
#' Produces an array with recruits by brood year, a data frame with both return year and brood year, and a data frame with ages and return year and brood year.
#'
#' Get recruits by brood year
#'
#' Calculate recruits by brood year, summing across ages.
#'
#' @param N Numeric, array of total run by population (i), year (y), and age (a).
#'
#' @return List with three elements. First element: numeric, array of recruits by  brood year with three dimensions: population (i), brood year (b).
#' Second element: data frame version of first element, plus return year. Third element: data frame of recruits by age, population, brood year and return year.
#'
#'
#' @examples
#'   R <- get_R( N = ex_N_total_run)
#'
#' @export
get_R <- function(N) {

  #populations <- dimnames(N)$i
  years <- dimnames(N)$y
  ages <- dimnames(N)$a
  #n_populations <- length(populations)
  n_ages <- length(ages)
  N_df <- array2DF( N, responseName = "N" )
  N_df$b <- as.integer(N_df$y) - as.integer(N_df$a)

  # Get complete brood years variable
  first_complete_brood <- min(as.integer(years)) - min(as.integer(ages))
  last_complete_brood <- max(as.integer(years)) - max(as.integer(ages))
  if(first_complete_brood <= last_complete_brood)
    complete_brood_years <- seq(first_complete_brood, last_complete_brood, by=1)
  # if first complete brood year is greater than last complete brood year, then there are no complete brood years.
  if(first_complete_brood > last_complete_brood )
    complete_brood_years <- NA
  N_df$complete_brood <- ifelse(N_df$b %in% complete_brood_years, TRUE, FALSE) # make complete brood cohort variable

  R_star_df <- N_df
  names(R_star_df) <- sub("^N$", "R_star", names(R_star_df))

  R_df <- aggregate( N ~ i + b +  complete_brood, data = N_df, FUN = sum)
  names(R_df) <- sub("^N$", "R", names(R_df))

  # Get array of recruits by population and brood year
  R <- df_to_array(R_df, value= "R", dimnames_order = c("i", "b"), default = 0, FUN = sum)

  res <- list(R = R, df = R_df, df_with_ages = R_star_df)
  res

}
