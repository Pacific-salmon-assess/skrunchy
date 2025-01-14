## Make up example data

# Exampled data is mostly made up, so don't use this for any analysis.
# Toy data for package testing and examples.
# Example data goes from return years 1984-2020.

library(abind)

# make up weekly catch and genetic proportion data
d <- make_P_G(catch_range = c(0,100), n_weeks = 12, n_years = NA, start_year = 1984, end_year = 2020,
              population_names = c("Kitsumkalum", "Lower Skeena", "Middle Skeena", "Zymoetz-Fiddler", "Large Lakes", "Upper Skeena"),
              seed = TRUE)
ex_P <- d$P
ex_sigma_P <- d$sigma_P
ex_G <- d$G

# get pooled genetic proportion data, annual
res <- get_P_tilde(P = d$P, sigma_P = d$sigma_P, G = d$G)
ex_P_tilde <- res$P_tilde
ex_sigma_P_tilde <- res$sigma_P_tilde

# Read in Kitsumkalum escapement data
ex_k <- read.csv(here("data-raw/kitsumkalum-escapement.csv"))

# get return to Terrace
X <- get_X(P_tilde = ex_P_tilde, sigma_P_tilde = ex_sigma_P_tilde , K= ex_k$kitsumkalum_escapement,
           sigma_K = ex_k$sd,
           y = ex_k$year)
ex_X <- X$X

# Make up Terminal mortality upstream of Terrace data
ex_Tau_U <- sample(50:100, size = length(ex_k$year), replace=TRUE)

# Get escapement
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

# Make up some age sample data
ad <- sapply(p_ages, FUN = function(x){ rpois( n = n_populations*n_years, lambda= x) })
ex_n <- array( ad,  dim = c(n_populations, n_years, n_ages), dimnames = list(i = populations, y = years, a = ages))

# Get age proportion data
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

# Get escapement by age
E_star <- get_E_star(E = ex_E, omega = ex_omega, K_star = ex_K_star, add_6_7 = TRUE)
ex_E_star <- E_star$E_star

# make up brood data
# Brood data by year and age
brood_ages <- c(4,5,6)
B_star <- array(sample(1:20, size= n_years*n_ages, replace=TRUE), dim = c(n_years, length(brood_ages)), dimnames = list( y = years, a = brood_ages))
#B_star[,"7"] <- 0 # make age 7 brood = 0 so you don't get negative fish in S_star
ex_B_star <- B_star
# Brood removals cannot be greater than escapement
ex_B_star[which( ex_E_star["Kitsumkalum",,] < ex_B_star )] <- 0

# brood data by year
B <- sample(40:50, size=n_years, replace=TRUE)
ex_B <- B

# Get spawner data
S_star <- get_S_star(E_star = ex_E_star, B_star = ex_B_star)
ex_S_star <- S_star$S_star

S <- get_S(method="sum_ages", S_star = ex_S_star)
ex_S <- S$S


# Make up hatchery contribution data
H_star <- S_star$S_star["Kitsumkalum",,] * runif(n = length(S_star$S_star["Kitsumkalum",,] ), 0.01, 0.2 )
ex_H_star <- H_star
ex_H <- apply(ex_H_star,1, sum )

W_star <- get_W_star(S_star = ex_S_star, H_star = ex_H_star,
    aggregate_population = "Skeena", hatchery_population = "Kitsumkalum")
ex_W_star <- W_star$W_star

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
usethis::use_data(ex_E_star, overwrite = TRUE)
usethis::use_data(ex_B_star, overwrite = TRUE)
usethis::use_data(ex_B, overwrite = TRUE)
usethis::use_data(ex_S_star, overwrite = TRUE)
usethis::use_data(ex_S, overwrite = TRUE)
usethis::use_data(ex_H_star, overwrite = TRUE)
usethis::use_data(ex_H, overwrite = TRUE)
usethis::use_data(ex_W_star, overwrite = TRUE)
usethis::use_data(ex_W_star, overwrite = TRUE)

# ex_data <- grep("^ex_.*", names(.GlobalEnv), value=TRUE)
# ex_data_list <- do.call("list", mget(ex_data))
# lapply(ex_data_list, FUN = function(x) {usethis::use_data(x, overwrite = TRUE)})


#usethis::use_data(P_tilde, overwrite = TRUE)
