#' Get age-specific escapement
#'
#' Calculate escapement of Chinook by population, year, and age.
#'
#' @param E Numeric, matrix of escapement values for Chinook with two dimensions: population (i), and year (y).
#' @param omega Numeric, array of proportions of each age with three dimensions: population (i), year (y), and age (a).
#' @param use_alternate_escapement_by_age Logical, should alternate escapement by age data be used for a population? Defaults to TRUE.
#' @param K_star Numeric, array of Kitsumkalum River escapement with two dimensions: return year (y) and age (a). Calculated using
#' age proportion data from spawning grounds (not from Tyee samples) for males and females separately.
#' @param population_use_age_from_river_samples Character, name of population to use alternate escapement by age data from sampling on spawning grounds. Defaults to Kitsumkalum.
#' @param add_6_7 If TRUE, add age 7 fish to age 6 fish of the same return year (treat age 7 returns as age 6 returns from the age 6 return year). Default is TRUE. Note that this "modifies" the brood year of age 7 fish (true brood year +1).
#' @param save_csv If TRUE, save a csv of the data frame output.
#' @param save_location Sub-directory folder of where to save csv
#' @param save_name File name of csv to save. Defaults to "E_star.csv"
#'
#' @return List with two elements. First element: numeric, array of escapement values with three dimensions: population (i), year (y), and age (a).
#'          Second element: data frame version of first element, for plotting and tables.
#'
#' @examples
#'   E_star <- get_E_star(E = ex_E, omega = ex_omega, K_star = ex_K_star, add_6_7 = TRUE)
#'
#' @export
get_E_star <- function(E, omega,
                       use_alternate_escapement_by_age = TRUE,
                       K_star,
                       population_use_age_from_river_samples = "Kitsumkalum",
                       add_6_7 = TRUE,
                       save_csv = FALSE, save_location,
                       save_name = "E_star.csv"
                       ) {
    E_star <- array(NA, dim = dim(omega), dimnames = dimnames(omega))
    n_years <- dim(omega)[2]
    populations <- dimnames(omega)$i
    populations_use_omega <- dimnames(E)$i[ !dimnames(E)$i == population_use_age_from_river_samples ]
    n_ages <- dim(omega)[3]
    for(y in 1:n_years) {
        for(a in 1:n_ages) {
        for(i in populations_use_omega) {
            E_star[ i ,y, a] <- E[i,y] * omega[i,y,a]
        }
        if( use_alternate_escapement_by_age == TRUE)
          E_star[ population_use_age_from_river_samples, y, a] <- K_star[y,a]
        }
    }
    if(add_6_7 == TRUE) { # add age 7 to age 6 fish by return year
      if(any(dimnames(E_star)$a == "7")) { # only do this if there are actual age 7 fish in the data
        E_star_add_6_7 <- E_star # new array to manipulate
        E_star_add_6_7[,,"6"] <- E_star_add_6_7[,,"6"] + E_star_add_6_7[,,"7"] # add age 7 escapements to age 6
        E_star_add_6_7 <- E_star_add_6_7[,,-grep("7", dimnames(E_star_add_6_7)$a)] # remove age 7 escapement dimension
        E_star <- E_star_add_6_7
      }
    }
    d <- as.data.frame.table(E_star, responseName = "E_star", stringsAsFactors = FALSE)
    d$y <- as.integer(d$y)
    d$a <- as.integer(d$a)
    d$b <- d$y - d$a
    if(save_csv == TRUE) {
      save_to <- here(save_location, save_name )
      write.csv(d, save_to, row.names = FALSE)
    }
    res <- list(E_star = E_star, df = d)
    res
}
