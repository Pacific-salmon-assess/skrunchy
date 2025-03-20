#' Estimate Chinook total terminal mortalities in the upper Skeena River, by year.
#'
#' Calculates the total terminal mortalities of Chinook salmon in the upper Skeena River
#' (upstream of Terrace), by year. Includes fishing plus incidental mortality.
#' Uses genetic data from Skeena Tyee test fishery, not CTC model outputs.
#'
#' See Winther et al. 2024 Appendix 2 https://publications.gc.ca/site/eng/9.901355/publication.html and Cox-Rogers et al. 1999 Appendix 2, Table 1 https://publications.gc.ca/site/eng/9.805331/publication.html for more info on incidental mortality rates.
#'
#' @param omega_J Numeric, array of proportions of each age including jacks (age 3) with three dimensions: population (i), year (y), and age (a). Note that this should include age proportion for Skeena aggregate.
#' @param rec_catch_U Numeric, vector of recreational catch of jack and adult Chinook in the upper Skeena (upstream of Terrace), by year.
#' @param IM_rec_catch Numeric, value to use for incidental mortality (drop-off) for recreational catch. Default is 6.9%.
#' @param FN_catch_U Numeric, vector of First Nations FSC catch of jack and adult Chinook in the upper Skeena (upstream of Terrace), by year.
#' @param IM_FN_catch Numeric, value to use for incidental mortality (drouput) for First Nations FSC catch. Default is 4.6%.
#' @param adult_ages Character, vector of adult ages to use. Should match age names of omega_J columns. Defaults to "4", "5", "6", and "7".
#'
#' @return A numeric vector of total terminal mortalities including catch and incidental mortalities, for the upper Skeena (upstream of Terrace), by year.
#'
#'
#' @examples
#' omega_J_all <- get_omega( ex_n_with_jacks ) # get age proportions with jacks
#' omega_J <- omega_J_all$omega["Skeena",,] # select just Skeena age proportions
#' Tau_U_total <- get_Tau_U_total( omega_J = omega_J,
#'                                rec_catch_U = ex_Tau$rec_catch_U,
#'                                FN_catch_U = ex_Tau$FN_catch_U )
#'
#' @export
get_Tau_U_total <- function( omega_J,
                             rec_catch_U, IM_rec_catch = 0.069,
                             FN_catch_U, IM_FN_catch = 0.046,
                             adult_ages = as.character(c(4,5,6,7))) {
  # Check vector lengths
  dim(omega_J)[1]
  length(rec_catch_U)
  length(FN_catch_U)

  years <- dimnames(omega_J)$y
  proportion_adults <- apply( omega_J[ , adult_ages], 1, FUN = sum)
  Tau_U <- proportion_adults * (   rec_catch_U * ( 1 + IM_rec_catch) +
                                   FN_catch_U * ( 1 + IM_FN_catch ))
  names(Tau_U) <- years
  return(Tau_U)
}

