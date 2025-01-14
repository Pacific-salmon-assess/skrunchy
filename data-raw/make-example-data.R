## code to prepare  dataset goes here

library(abind)

d <- make_P_G(catch_range = c(0,100), n_weeks = 12, n_years = NA, start_year = 1984, end_year = 2023,
              population_names = c("Kitsumkalum", "Lower Skeena", "Middle Skeena", "Zymoetz-Fiddler", "Large Lakes", "Upper Skeena"),
              seed = TRUE)
ex_P <- d$P
ex_sigma_P <- d$sigma_P
ex_G <- d$G

res <- get_P_tilde(P = d$P, sigma_P = d$sigma_P, G = d$G)
ex_P_tilde <- res$P_tilde
ex_sigma_P_tilde <- res$sigma_P_tilde

ex_k <- read.csv(here("data-raw/kitsumkalum-escapement.csv"))

ex_X <- get_X(P_tilde = ex_P_tilde, sigma_P_tilde = ex_sigma_P_tilde , K= ex_K,
           sigma_K = ex_sigma_K,
           y = k$year)
ex_Tau_U <- sample(50:100, size = length(ex_k$year), replace=TRUE)


# Save example data sets to data/ as .rda files
usethis::use_data(ex_P, overwrite = TRUE)
usethis::use_data(ex_sigma_P, overwrite = TRUE)
usethis::use_data(ex_G, overwrite = TRUE)
usethis::use_data(ex_P_tilde, overwrite = TRUE)
usethis::use_data(ex_sigma_P_tilde, overwrite = TRUE)
usethis::use_data(ex_k, overwrite = TRUE)
usethis::use_data(ex_X, overwrite = TRUE)
usethis::use_data(ex_Tau_U, overwrite = TRUE)

#usethis::use_data(P_tilde, overwrite = TRUE)
