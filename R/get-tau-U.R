#' Estimate Chinook total terminal mortalities in the upper Skeena River
#'
#' Calculates the total terminal mortalities of Chinook salmon in the upper Skeena River
#' (upstream of Terrace), by population, year, and age. Includes fishing plus incidental mortality.
#' Uses genetic data from Skeena Tyee test fishery, not CTC model outputs.
#'
#' @param Tau_U Numeric, vector of total terminal mortalities in the upper Skeena (upstream of Terrace), by year. Combination of FSC and recreational. Note uppercase Tau denotes total not broken out by population or age.
#' @param omega Numeric, array of proportions of each age with three dimensions: population (i), year (y), and age (a).
#' @param P_tilde Numeric, array of genetic proportions at Skeena Tyee test fishery with 2 dimensions: i (population) and year (y).
#' @param aggregate_population Character, name of aggregate population. Defaults to "Skeena"
#' @param upper_populations Character, vector of names of upper populations. Defaults to Middle Skeena, Large Lakes, and Upper Skeena.
#' @param lower_populations Character, vector of names of lower populations. Defaults to Lower Skeena, Kitsumkalum, and Zymoetz-Fiddler.
#'
#' @return A list with two objects. First object: numeric, array of total terminal mortalities in the upper Skeena with three dimensions: population (i), year (y), and age (a).
#'        Second object: data frame with same data in long format (columns for population, year, and age) for plotting.
#'
#' @examples
#' tau_U <- get_tau_U( Tau_U = ex_Tau_U_total, omega = ex_omega, P_tilde = ex_P_tilde,
#'     aggregate_population = "Skeena", upper_populations = c("Middle Skeena", "Large Lakes", "Upper Skeena"),
#'     lower_populations = c("Lower Skeena", "Kitsumkalum", "Zymoetz-Fiddler") )
#'
#' @export
get_tau_U <- function( Tau_U, omega, P_tilde, aggregate_population = "Skeena",
                       upper_populations = c("Middle Skeena", "Large Lakes", "Upper Skeena"),
                       lower_populations = c("Lower Skeena", "Kitsumkalum", "Zymoetz-Fiddler") ) {
  # sum the proportions of the 6 summer run populations upstream of Tyee test fishery by year
  sum_P_tilde_upper <- apply(P_tilde[ upper_populations,], 2, sum)
  # make array to fill in
  tau_U <- array( rep(NA,length(omega)), dim=dim(omega), dimnames = dimnames(omega))
  n_ages <- dim(omega)[3]
  n_years <- dim(omega)[2]
  # Fill in with total mortality estimates
  for(a in 1:n_ages) {
    for(y in 1:n_years) {
    tau_U[aggregate_population, y, a] <- Tau_U[y] * omega[aggregate_population, y, a]
    for(i in upper_populations) {
      tau_U[i, y, a] <- Tau_U[y] * omega[i, y, a] * P_tilde[i,y] / sum_P_tilde_upper[y]
    }
    tau_U[lower_populations, y, a] <- 0
    }
  }
  d <- as.data.frame.table(tau_U, responseName = "tau_U")
  d$y <- as.integer(as.character(d$y))
  res <- list( tau_U = tau_U, df = d )
  res
}




