#' Estimate Chinook total terminal mortalities in the upper Skeena River, by population, year, and age.
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
#' @param add_6_7 Logical, If TRUE, add age 7 fish to age 6 fish of the same brood year (treat age 7 mortalities as age 6 mortalities from the age 6 brood year). Default is TRUE. Note that this "modifies" the return year of age 7 mortalities (true return year -1).
#'
#' @return A list with two objects. First object: numeric, array of total terminal mortalities in the upper Skeena with three dimensions: population (i), year (y), and age (a).
#'        Second object: data frame with same data in long format (columns for population, year, and age) for plotting.
#'
#' @examples
#' tau_U <- get_tau_U( Tau_U = ex_Tau_U_total, omega = ex_omega,
#'                     P_tilde = ex_P_tilde,
#'                     aggregate_population = "Skeena",
#'                     upper_populations = c("Middle Skeena", "Large Lakes",
#'                             "Upper Skeena"),
#'                     lower_populations = c("Lower Skeena", "Kitsumkalum",
#'                                         "Zymoetz-Fiddler"),
#'                     add_6_7 = TRUE )
#'
#' @export
get_tau_U <- function( Tau_U, omega, P_tilde, aggregate_population = "Skeena",
                       upper_populations = c("Middle Skeena", "Large Lakes", "Upper Skeena"),
                       lower_populations = c("Lower Skeena", "Kitsumkalum", "Zymoetz-Fiddler"),
                       add_6_7 = TRUE) {
  # Check year and population
  # Years
  if(!isTRUE( identical(length(Tau_U), dim(omega)[2]) && identical(length(Tau_U) , dim(P_tilde)[2]  )))  {
    stop("Length of year (y) dimensions not equal.") }
  if(!all( dimnames(omega)$y %in% dimnames(P_tilde)$y )) {
    stop("Year (y) values are not equal.")    }
  # Populations
  # FLAG: note that if omega has age proportion data from Skeena aggregate, it will have one more population than P_tilde, which
  # does not contain the Skeena aggregate, since it is by definition proportions of the Skeena aggregate. If omega doesn't have
  # age proportion data for the Skeena aggregate, this check will throw an error.
  if(!identical( dim(omega)[1], as.integer(dim(P_tilde)[1] +1))) {
    stop("Length of population (i) dimensions not equal.") }
  # have to exclude Skeena aggregate from omega.
  if(!all(dimnames(omega)$i[!dimnames(omega)$i =="Skeena"] %in% dimnames(P_tilde)$i )) {
    stop("Population (i) values are not equal.")    }

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
  if(add_6_7 == TRUE) { # add age 7 to age 6 fish by brood year
    if(any(dimnames(tau_U)$a == "7")) { # only do this if there are actual age 7 fish in the data
      tau_U_add_6_7 <- tau_U # new array to manipulate
      # Add age 7 mortalities to age 6 mortalities by brood year (use age 7 mortalities from return year +1)
      for(y in 1:(n_years-1)) {
        tau_U_add_6_7[,y,"6"] <- tau_U_add_6_7[,y,"6"] + tau_U_add_6_7[,y+1,"7"] # add age 7 mortalities to age 6 by brood year (age 7 mortalities from age 6 return year +1)
      }
      tau_U_add_6_7[,n_years,"6"] <- tau_U_add_6_7[,n_years,"6"] # for last year, no age 7s to add
      tau_U_add_6_7 <- tau_U_add_6_7[,,-grep("7", dimnames(tau_U_add_6_7)$a)] # remove age 7 escapement dimension
      tau_U <- tau_U_add_6_7
    }
  }
  d <- as.data.frame.table(tau_U, responseName = "tau_U")
  d$y <- as.integer(as.character(d$y))
  d$a <- as.integer(as.character(d$a))
  res <- list( tau_U = tau_U, df = d )
  res
}




