#' Calculate Chinook returns to Terrace
#'
#' Calculate returns to Terrace of adult Chinook salmon for population i and year y. Uses pooled genetic proportions and the number of Chinook returns to the Kitsumkalum River.
#'
#' @param P_tilde Numeric, array of genetic proportions at Skeena Tyee test fishery with 2 dimensions: i (population) and year (y).
#' @param sigma_P_tilde Numeric, array of SD of pooled genetic proportions with 2 dimensions: i (population) and y (year).
#' @param K Numeric, vector of estimates of Kitsumkalum Chinook spawners (ages 4, 5, 6, and 7, no jacks) based on a mark-recapture study and open population mark-recapture model (POPAN). One dimension: y (year). For more on POPAN models, see Cooch & White 2024 Chapter 12: http://www.phidot.org/software/mark/docs/book/pdf/chap12.pdf
#' @param sigma_K Numeric, vector of standard error (SE) of K. One dimension: y (year).
#' @param y Integer, vector of years for Kitsumkalum Chinook spawners K and sigma_K.
#'
#' @return Numeric, an array of
#' @examples
#'
#'
#'
#' #' @export
get_T <- function(P_tilde, sigma_P_tilde, K, sigma_K, y) {

}
