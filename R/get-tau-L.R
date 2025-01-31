#' Estimate Chinook total terminal mortalities in the lower Skeena River, by population, year, and age.
#'
#' Calculates the total terminal mortalities of Chinook salmon in the lower Skeena River
#' (downstream of Terrace), by population, year, and age. Includes fishing plus incidental mortality.
#' Uses genetic data from Skeena Tyee test fishery, not CTC model outputs.
#'
#' @param Tau_L Numeric, vector of total terminal mortalities in the lower Skeena (downstream of Terrace), by year. Combination of FSC and recreational. Note uppercase Tau denotes total not broken out by population or age.
#' @param omega Numeric, array of proportions of each age with three dimensions: population (i), year (y), and age (a). Note that this should include age proportion for Skeena aggregate.
#' @param P_tilde Numeric, array of genetic proportions at Skeena Tyee test fishery with 2 dimensions: i (population) and year (y).
#' @param aggregate_population Character, name of aggregate population
#' @param add_6_7 Logical, If TRUE, add age 7 fish to age 6 fish of the same brood year (treat age 7 mortalities as age 6 mortalities from the age 6 brood year). Default is TRUE. Note that this "modifies" the return year of age 7 mortalities (true return year -1).
#'
#' @return A list with two objects. First object: numeric, array of total terminal mortalities in the lower Skeena with three dimensions: population (i), year (y), and age (a).
#'        Second object: data frame with same data in long format (columns for population, year, and age) for plotting.
#'
#' @examples
#' tau_L <- get_tau_L( Tau_L = ex_Tau_L_total, omega = ex_omega,
#'                 P_tilde = ex_P_tilde, aggregate_population = "Skeena",
#'                 add_6_7 = TRUE)
#'
#' @export
get_tau_L <- function( Tau_L, omega, P_tilde, aggregate_population = "Skeena",
                       add_6_7= TRUE) {
  # Check year and population
  # Years
  if(!isTRUE( identical(length(Tau_L), dim(omega)[2]) && identical( length(Tau_L), dim(P_tilde)[2]  )))  {
    stop("Length of year (y) dimensions not equal.") }
  if(!all( dimnames(omega)$y %in% dimnames(P_tilde)$y )) {
    stop("Year (y) values are not equal.")    }
  # Populations
  # FLAG: note that if omega has age proportion data from Skeena aggregate, it will have one more population than P_tilde, which
     # does not contain the Skeena aggregate, since it is by definition proportions of the Skeena aggregate. If omega doesn't have
    # age proportion data for the Skeena aggregate, this check will throw an error.
  if(!isTRUE(identical(dim(omega)[1], as.integer(dim(P_tilde)[1] +1)))) {
    stop("Length of population (i) dimensions not equal.") }
  # have to exclude Skeena aggregate from omega.
  if(!all(dimnames(omega)$i[!dimnames(omega)$i =="Skeena"] %in% dimnames(P_tilde)$i )) {
    stop("Population (i) values are not equal.")    }

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
  if(add_6_7 == TRUE) { # add age 7 to age 6 fish by brood year
    if(any(dimnames(tau_L)$a == "7")) { # only do this if there are actual age 7 fish in the data
      tau_L_add_6_7 <- tau_L # new array to manipulate
      # Add age 7 mortalities to age 6 mortalities by brood year (use age 7 mortalities from return year +1)
      for(y in 1:(n_years-1)) {
        tau_L_add_6_7[,y,"6"] <- tau_L_add_6_7[,y,"6"] + tau_L_add_6_7[,y+1,"7"] # add age 7 mortalities to age 6 by brood year (age 7 mortalities from age 6 return year +1)
      }
      tau_L_add_6_7[,n_years,"6"] <- tau_L_add_6_7[,n_years,"6"] # for last year, no age 7s to add
      tau_L_add_6_7 <- tau_L_add_6_7[,,-grep("7", dimnames(tau_L_add_6_7)$a)] # remove age 7 escapement dimension
      tau_L <- tau_L_add_6_7
    }
  }
  d <- as.data.frame.table(tau_L, responseName = "tau_L")
  d$y <- as.integer(as.character(d$y))
  d$a <- as.integer(as.character(d$a))
  res <- list( tau_L = tau_L, df = d )
  res
}




