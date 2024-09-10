#' Get standard error for X, return to Terrace
#'
#' @param X Array of returns to Terrace with two dimensions: population (i) and year (y).
#' @param K Numeric, vector of estimates of Kitsumkalum Chinook spawners (ages 4, 5, 6, and 7, no jacks) based on a mark-recapture study and open population mark-recapture model (POPAN). One dimension: y (year). For more on POPAN models, see Cooch & White 2024 Chapter 12: http://www.phidot.org/software/mark/docs/book/pdf/chap12.pdf
#' @param sigma_K Numeric, vector of standard error (SE) of K. One dimension: y (year).
#' @param P_tilde Numeric, array of genetic proportions at Skeena Tyee test fishery with 2 dimensions: i (population) and year (y).
#' @param sigma_P_tilde Numeric, array of SD of pooled genetic proportions with 2 dimensions: i (population) and y (year).
#' @param aggregate_population Logical, should the SD for the aggregate population be calculated?
#'
#' @return Array, numeric, SE of returns to Terrace of Chinook, with two dimensions: population (i) and year (y).
#'
#' @examples
#'
#' sigma_X <- get_sigma_X( X = 60000,
#'              K = 10000, sigma_K = 500,
#'              P_tilde = 0.2,
#'              sigma_P_tilde = 0.05,
#'              aggregate_population = TRUE)
#' sigma_X
#'
#' @export
get_sigma_X <- function( X, K, sigma_K, P_tilde, sigma_P_tilde, aggregate_population = TRUE) {
   if(aggregate_population == TRUE) {
        variance <- X^2 * ( sigma_K / K^2 + sigma_P_tilde / P_tilde^2 )
        SE <- sqrt(variance)
        SE
   } else {
    # still waiting to hear from Ivan on how SE is calculated for non-Skeena populations. Not in report
     NA
  }

}
