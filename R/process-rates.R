#' Process rate data by replacing high values with a rolling mean value.
#'
#' Takes rate data from CTC exploitation rate analysis (or any rate data) and
#' applies a threshold of a high value to it. Values above this upper limit are
#' replaced with a rolling mean that is calculated after dropping high values.
#'
#' @param rate_array Numeric, array of exploitation rates. Two dimensions: y (return year) and a (age).
#' @param upper_limit Numeric, upper limit used to determine which rates are dropped and replaced with rolling mean. Default is 0.5.
#' @param roll_mean_window Numeric, how many values to use when applying rolling mean. Default is 5.
#'
#' @returns Array, high values replaced with means.
#'
#'
#' @examples
#' years <- 1984:2020
#' ages <- 4:6
#' d <- matrix( rbeta( n = length(years)*length(ages), shape1 = 2, shape2 = 5 ), nrow = length(years))
#' names(d)
#' dimnames(d) <- list( "y" = years, "a" = ages )
#' dn <- process_rates( d )
#' plot( d ~ dn )
#'
#' @export
process_rates <- function( rate_array, upper_limit = 0.5, roll_mean_window = 5 ) {
  arr <- rate_array
  # make array with NA for values >0.5, for rolling mean
  arr_drop_hi_vals <- ifelse(arr < 0.5, arr, NA)
  # running mean of drop hi vals array
  arr_rollmean <- rollapply( arr_drop_hi_vals, width = 5, FUN = mean,
                                  by.column = TRUE,
                                  na.rm=TRUE, fill = NA, partial = TRUE )
  dimnames( arr_rollmean ) <- dimnames( arr_drop_hi_vals ) # not necessary, but useful for plotting to check
  # sub rollmean for hi values
  arr_new <- ifelse( arr < 0.5, arr, arr_rollmean)
  return(arr_new)
}
