#' Estimate Chinook total terminal mortalities in the lower Skeena River, by year.
#'
#' Calculates the total terminal mortalities of Chinook salmon in the lower Skeena River
#' (downstream of Terrace), by year. Includes fishing plus incidental mortality.
#' Uses genetic data from Skeena Tyee test fishery, not CTC model outputs.
#'
#' See Winther et al. 2024 Appendix 2 https://publications.gc.ca/site/eng/9.901355/publication.html and Cox-Rogers et al. 1999 Appendix 2, Table 1 https://publications.gc.ca/site/eng/9.805331/publication.html for more info on incidental mortality rates.
#'
#' @param omega_J Numeric, array of proportions of each age including jacks (age 3) for the Skeena River aggregate, with two dimensions: year (y), and age (a).
#' Note that this should include age proportion for Skeena aggregate. This is used to remove jacks from the terminal mortality.
#' @param tyee Numeric, vector of the total catch of adult and jack Chinook at the Skeena Tyee Test fishery, by year.
#' @param IM_tyee Numeric, value to use for incidental mortality (drouput) for Tyee test fishery. Default is 4.6%.
#' @param rec_catch_L Numeric, vector of recreational catch of jack and adult Chinook in the lower Skeena (downstream of Terrace), by year.
#' @param IM_rec_catch Numeric, value to use for incidental mortality (drop-off) for recreational catch. Default is 6.9%.
#' @param rec_release_L Numeric, vector of recreational catch of jack and adult Chinook in the lower Skeena (downstream of Terrace), by year.
#' @param IM_rec_release Numeric, value to use for incidental mortality (release mortality) for recreational catch. Default is 5%.
#' @param FN_catch_L Numeric, vector of First Nations FSC catch of jack and adult Chinook in the lower Skeena (downstream of Terrace), by year.
#' @param IM_FN_catch Numeric, value to use for incidental mortality (drouput) for First Nations FSC catch. Default is 4.6%.
#' @param adult_ages Character, vector of adult ages to use. Should match age names of omega_J columns. Defaults to "4", "5", "6", and "7".
#'
#' @return A numeric vector of total terminal mortalities including catch and incidental mortalities, for the lower Skeena (downstream of Terrace), by year.
#'
#' @examples
#' omega_J_all <- get_omega( ex_n_with_jacks ) # get age proportions with jacks
#' omega_J <- omega_J_all$omega["Skeena",,] # select just Skeena age proportions
#' Tau_L_total <- get_Tau_L_total( omega_J = omega_J, tyee = ex_Tau_total$tyee,
#'                                rec_catch_L = ex_Tau_total$rec_catch_L,
#'                                rec_release_L = ex_Tau_total$rec_release_L,
#'                                FN_catch_L = ex_Tau_total$FN_catch_L )
#'
#' @export
get_Tau_L_total <- function( omega_J,
                             tyee, IM_tyee = 0.046,
                             rec_catch_L, IM_rec_catch = 0.069,
                             rec_release_L, IM_rec_release = 0.05,
                             FN_catch_L, IM_FN_catch = 0.046,
                             adult_ages = as.character(c(4,5,6,7))) {
  # Check vector lengths

    years <- dimnames(omega_J)$y
    proportion_adults <- apply( omega_J[ , adult_ages], 1, FUN = sum)
    Tau_L <- proportion_adults * ( tyee * ( 1  + IM_tyee ) +
                                   rec_catch_L * ( 1 + IM_rec_catch) +
                                   rec_release_L * IM_rec_release +
                                   FN_catch_L * ( 1 + IM_FN_catch ))
    names(Tau_L) <- years
    return(Tau_L)
}
