#' Get age-specific escapement for Kitsumkalum escapement
#'
#' Calculate escapement of Chinook to the Kitsumkalum River by year and age. Uses age
#' proportions for males and females from scales collected during tagging program on the Kitsumkalum River (not Tyee test fishery).
#' Applies male and female age proportions to male and female escapement estimates from mark-recapture program and POPAN analysis.
#' Also uses age-specific hatchery component. Assumes sex ratio is equal in wild and hatchery fish in a given year.
#'
#' @param K Numeric, escapement of total Kitsumkalum Chinook (males and females combined).
#' @param y_K Integer, vector of years for Kitsumkalum Chinook spawners K and sigma_K.
#' @param K_M Numeric, escapement of male Kitsumkalum Chinook.
#' @param K_F Numeric, escapement of female Kitsumkalum Chinook.
#' @param omega_KM Numeric, array of proportions of each age for males, with two dimensions: year (y), and age (a).
#' @param omega_KF Numeric, array of proportions of each age for females, with two dimensions: year (y), and age (a).
#' @param H numeric, number of hatchery fish in escapement by return year. From CTC model outputs.
#' @param H_star numeric, array of hatchery fish in escapement with two dimensions: year (y) and age (a). From CTC model outputs.
#' @param save_csv If TRUE, save a csv of the data frame output.
#' @param save_location Sub-directory folder of where to save csv
#' @param save_name File name of csv to save. Defaults to "E_star.csv"
#'
#' @return List with two elements. First element: numeric, array of Kitsumkalum River escapement with two dimensions: return year (y) and age (a).
#'          Second element: data frame version of first element, for plotting and tables.
#'
#' @examples
#'   K_star <- get_K_star(K = ex_k$kitsumkalum_escapement, y_K = ex_k$year,
#'                       K_M  = ex_k$male_escapement, K_F = ex_k$female_escapement,
#'                       omega_KM = ex_omega_KM, omega_KF = ex_omega_KF,
#'                       H = ex_H, H_star = ex_H_star )
#'
#' @export
get_K_star <- function(K, y_K, K_M, K_F, omega_KM, omega_KF, H, H_star,
                       save_csv = FALSE, save_location,
                       save_name = "K_star.csv") {

  # Add a column for age 7 hatchery fish and fill with zeros, add name
  H_star1 <- cbind( H_star, 0)
  dimnames(H_star1)[[2]][4] <- "7"

  # Age check
  if(!isTRUE(identical( dim(omega_KM)[2], dim(omega_KF)[2]) && identical(dim(omega_KM)[2], dim(H_star1)[2]))) {
    stop("Length of age (a) dimensions not equal.") }
  if(!all(dimnames(omega_KM)$a %in% dimnames(omega_KF)$a , dimnames(omega_KM)$a %in% dimnames(H_star1)[[2]] )) {
    stop("Age (a) values are not equal.") }
  # Year check
  if(!isTRUE(identical( length(K), length(y_K)) && identical(length(K), length(K_M)) &&
             identical(length(K), length(K_F)) &&   identical(length(K), length(H)) &&
             identical(length(K), dim(omega_KM)[1])   &&   identical(length(K), dim(omega_KF)[1] ) &&
             identical(length(K), dim(H_star)[1] ) ))  {
    stop("Length of year (y) dimensions not equal.") }
  if(!all(dimnames(omega_KM)$y %in% dimnames(omega_KF)$y , dimnames(omega_KM)$y %in% dimnames(H_star1)[[1]] )) {
    stop("Year (y) values are not equal.") }

  # Main function
  K_star <- array(NA, dim = dim(omega_KM), dimnames = dimnames(omega_KM))
  n_years <- dim(omega_KM)[1]
  n_ages <- dim(omega_KM)[2]
  for(y in 1:n_years) {
    for(a in 1:n_ages) {
        K_star[ y, a] <- omega_KM[y, a] * ( K_M[y] - H[y] * ( K_M[y] / K[y] )) +
                         omega_KF[y, a] * ( K_F[y] - H[y] * ( K_F[y] / K[y] )) +
                         H_star1[y, a]
    }
  }
  d <- as.data.frame.table(K_star, responseName = "K_star", stringsAsFactors = FALSE)
  d$y <- as.integer(d$y)
  d$a <- as.integer(d$a)
  if(save_csv == TRUE) {
    save_to <- here(save_location, save_name )
    write.csv(d, save_to, row.names = FALSE)
  }
  res <- list(K_star = K_star, df = d)
  res
}
