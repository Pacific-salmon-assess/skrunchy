## code to prepare  dataset goes here

# Exampled data is mostly made up, so don't use this for any analysis.
# Toy data for package testing and examples.
# Example data goes from return years 1984-2020.

library(abind)

d <- make_P_G(catch_range = c(0,100), n_weeks = 12, n_years = NA, start_year = 1984, end_year = 2020,
              population_names = c("Kitsumkalum", "Lower Skeena", "Middle Skeena", "Zymoetz-Fiddler", "Large Lakes", "Upper Skeena"),
              seed = TRUE)
ex_P <- d$P
ex_sigma_P <- d$sigma_P
ex_G <- d$G

res <- get_P_tilde(P = d$P, sigma_P = d$sigma_P, G = d$G)
ex_P_tilde <- res$P_tilde
ex_sigma_P_tilde <- res$sigma_P_tilde

ex_k <- read.csv(here("data-raw/kitsumkalum-escapement.csv"))

X <- get_X(P_tilde = ex_P_tilde, sigma_P_tilde = ex_sigma_P_tilde , K= ex_k$kitsumkalum_escapement,
           sigma_K = ex_k$sd,
           y = ex_k$year)
ex_X <- X$X
ex_Tau_U <- sample(50:100, size = length(ex_k$year), replace=TRUE)

E <- get_E(K = ex_k$kitsumkalum_escapement, X = ex_X, Tau_U = ex_Tau_U,
   known_population = "Kitsumkalum",
    aggregate_population = "Skeena",
    lower_populations = c("Lower Skeena", "Zymoetz-Fiddler"),
    upper_populations = c("Upper Skeena", "Middle Skeena", "Large Lakes"))
ex_E <- E$E
populations <- dimnames(ex_X)$i
n_populations <- length(populations)
years <- dimnames(ex_X)$y
n_years <- length(years)
ages <- c(4,5,6,7)
p_ages <- c(30,40,40,1)
n_ages <- length(ages)
# Make up some age data
ad <- sapply(p_ages, FUN = function(x){ rpois( n = n_populations*n_years, lambda= x) })
ex_n <- array( ad,  dim = c(n_populations, n_years, n_ages), dimnames = list(i = populations, y = years, a = ages))
omega <- get_omega(ex_n)
ex_omega <- omega$omega


# read in Kitsumkalum age-specific escapement
ksd <- read.csv(here("data-raw", "K_star.csv"))
ksdl <- ksd %>% tidyr::pivot_longer(c("X4", "X5", "X6", "X7"), names_to = "a", values_to = "K_star")
ksdl$a <- sub("X", "", ksdl$a)
dim_order_K_star <- c("y","a")
dim_position_K_star <- sapply(dim_order_K_star, function(x) grep( paste0("^", x, "$"), names(ksdl)))
K_star <- tapply(ksdl$K_star, ksdl[ , dim_position_K_star], FUN=print, default = 0)
ex_K_star <- K_star

# Save example data sets to data/ as .rda files
usethis::use_data(ex_P, overwrite = TRUE)
usethis::use_data(ex_sigma_P, overwrite = TRUE)
usethis::use_data(ex_G, overwrite = TRUE)
usethis::use_data(ex_P_tilde, overwrite = TRUE)
usethis::use_data(ex_sigma_P_tilde, overwrite = TRUE)
usethis::use_data(ex_k, overwrite = TRUE)
usethis::use_data(ex_X, overwrite = TRUE)
usethis::use_data(ex_Tau_U, overwrite = TRUE)
usethis::use_data(ex_E, overwrite = TRUE)
usethis::use_data(ex_n, overwrite = TRUE)
usethis::use_data(ex_omega, overwrite = TRUE)
usethis::use_data(ex_K_star, overwrite = TRUE)

#usethis::use_data(P_tilde, overwrite = TRUE)
