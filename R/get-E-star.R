#' Get age-specific escapement
#'
#' Calculate escapement of Chinook by population, year, and age.
#'
#' @param E Numeric, matrix of escapement values for Chinook with two dimensions: population (i), and year (y).
#' @param omega Numeric, array of proportions of each age with three dimensions: population (i), year (y), and age (a).
#' @param save_csv If TRUE, save a csv of the data frame output.
#' @param add_6_7 If TRUE, add age 7 fish to age 6 fish of the same return year (treat age 7 returns as age 6 returns from the age 6 return year).
#'
#' @return List with two elements. First element: numeric, array of escapement values with three dimensions: population (i), year (y), and age (a).
#'          Second element: data frame version of first element, for plotting and tables.
#'
#' @examples
#' library(abind)
#' d <- make_P_G(start_year = 2000, end_year = 2001)
#' res <- get_P_tilde(P = d$P, sigma_P = d$sigma_P, G = d$G)
#' k <- data.frame( year = c(2000, 2001),
#'                 kitsumkalum_escapement = c(5000, 6000),
#'                 sd = c(500, 400))
#' X <- get_X(P_tilde = res$P_tilde, sigma_P_tilde = res$sigma_P_tilde, K= k$kitsumkalum_escapement,
#'           sigma_K = k$sd, y = k$year)
#' Tau_U <- sample(50:100, size = length(k$year), replace=TRUE)
#' E <- get_E(K = k$kitsumkalum_escapement, X = X$X, Tau_U = Tau_U,
#'    known_population = "Kitsumkalum",
#'     aggregate_population = "Skeena",
#'     lower_populations = c("Lower Skeena", "Zymoetz-Fiddler"),
#'     upper_populations = c("Upper Skeena", "Middle Skeena", "Large Lakes"))
#'   populations <- c("Kitsumkalum", "Lower Skeena", "Zymoetz-Fiddler", "Upper Skeena", "Middle Skeena", "Large Lakes", "Skeena")
#'   n_populations <- length(populations)
#'   years <- 2000:2001
#'   n_years <- length(years)
#'   ages <- c(4,5,6,7)
#'   p_ages <- c(30,40,40,1)
#'   n_ages <- length(ages)
#'   # Make up some age data
#'   d <- sapply(p_ages, FUN = function(x){ rpois( n = n_populations*n_years, lambda= x) })
#'   n <- array( d,  dim = c(n_populations, n_years, n_ages), dimnames = list(i = populations, y = years, a = ages))
#'   omega <- get_omega(n)
#'   E_star <- get_E_star(E = E$E, omega = omega$omega)
#'
#' @export
get_E_star <- function(E, omega,
                       save_csv = FALSE,
                       add_6_7 = TRUE) {
    E_star <- array(NA, dim = dim(omega), dimnames = dimnames(omega))
    n_years <- dim(omega)[2]
    populations <- dimnames(omega)$i
    n_ages <- dim(omega)[3]
    for(y in 1:n_years) {
      for(i in populations) {
        for(a in 1:n_ages) {
        E_star[ i,y, a] <- E[i,y] * omega[i,y,a]
        }
      }
    }
    if(add_6_7 == TRUE) {
      if(any(dimnames(E_star)$a == "7")) {
        E_star_add_6_7 <- E_star # new array to manipulate
        E_star_add_6_7[,,"6"] <- E_star[,,"6"] + E_star[,,"7"] # add age 7 escapements to age 6
        E_star_add_6_7 <- E_star[,,1:(n_ages-1)] # remove age 7 escapement dimension
        E_star <- E_star_add_6_7
      }
    }
    d <- as.data.frame.table(E_star, responseName = "E_star", stringsAsFactors = FALSE)
    d$y <- as.integer(d$y)
    d$a <- as.integer(d$a)
    d$b <- d$y - d$a
    if(save_csv == TRUE) {
      write.csv(d, here("data-out/E_star.csv"), row.names = FALSE)
    }
    res <- list(E_star = E_star, df = d)
    res
}
