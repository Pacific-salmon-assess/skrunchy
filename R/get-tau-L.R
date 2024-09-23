#' Estimate Chinook total terminal mortalities in the lower Skeena River
#'
#' Calculates the total terminal mortalities of Chinook salmon in the lower Skeena River
#' (downstream of Terrace), by population, year, and age. Includes fishing plus incidental mortality.
#' Uses genetic data from Skeena Tyee test fishery, not CTC model outputs.
#'
#' @param Tau_L Numeric, vector of total terminal mortalities in the lower Skeena (downstream of Terrace), by year. Combination of FSC and recreational. Note uppercase Tau denotes total not broken out by population or age.
#' @param omega Numeric, array of proportions of each age with three dimensions: population (i), year (y), and age (a).
#' @param P_tilde Numeric, array of genetic proportions at Skeena Tyee test fishery with 2 dimensions: i (population) and year (y).
#' @param aggregate_population Character, name of aggregate population
#'
#' @return A list with two objects. First object: numeric, array of total terminal mortalities in the lower Skeena with three dimensions: population (i), year (y), and age (a).
#'        Second object: data frame with same data in long format (columns for population, year, and age) for plotting.
#'
#' @examples
#' populations <- c("Kitsumkalum", "Lower Skeena", "Zymoetz-Fiddler", "Upper Skeena", "Middle Skeena", "Large Lakes", "Skeena")
#' n_populations <- length(populations)
#' years <- 2000:2001
#' n_years <- length(years)
#' ages <- c(3,4,5,6,7)
#' p_ages <- c(20,30,40,40,1)
#' n_ages <- length(ages)
#' d <- sapply(p_ages, FUN = function(x){ rpois( n = n_populations*n_years, lambda= x) })
#' n <- array( d,  dim = c(n_populations, n_years, n_ages), dimnames = list(i = populations, y = years, a = ages))
#' omega <- get_omega(n)
#' Tau_L <- sample(500:1000, size=n_years)
#' P_G <-  make_P_G(, start_year = 2000, n_years = n_years, population_names = populations[1:6])
#' P_tilde <- get_P_tilde( P = P_G$P, sigma_P = P_G$sigma_P, G = P_G$G)
#' tau_L <- get_tau_L( Tau_L = Tau_L, omega = omega$omega, P_tilde = P_tilde$P_tilde, aggregate_population = "Skeena")
#'
#' @export
get_tau_L <- function( Tau_L, omega, P_tilde, aggregate_population = "Skeena") {
  # get character vector of populations that are not the aggregate
  not_aggregate_populations <- dimnames(omega)$i[ !dimnames(omega)$i == aggregate_population]
  # sum the proportions of the 6 summer run populations upstream of Tyee test fishery by year
  sum_P_tilde<- apply(P_tilde[ not_aggregate_populations,], 2, sum)
  # make array to fill in
  tau_L <- array( rep(NA,length(omega)), dim=dim(omega), dimnames = dimnames(omega))
  n_ages <- dim(omega)[3]
  n_years <- dim(omega)[2]
  # Fill in with total mortality estimates
  for(a in 1:n_ages) {
    for(y in 1:n_years) {
    tau_L[aggregate_population, y, a] <- Tau_L[y] * omega[aggregate_population, y, a]
    tau_L[not_aggregate_populations, y, a] <- Tau_L[y] * omega[not_aggregate_populations, y, a] * P_tilde[not_aggregate_populations, y] / sum_P_tilde[y]
    }
  }
  d <- as.data.frame.table(tau_L, responseName = "tau_L")
  d$y <- as.integer(as.character(d$y))
  res <- list( tau_L = tau_L, df = d )
  res
}




