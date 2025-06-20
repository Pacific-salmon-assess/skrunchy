## Make up example data

# Exampled data is mostly made up, so don't use this for any analysis.
# Toy data for package testing and examples.
# Example data goes from return years 1984-2020.

library(abind)
library(skrunchy)

devtools::load_all()

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
           y_K = ex_k$year)
ex_X <- X$X

populations <- dimnames(ex_X)$i
n_populations <- length(populations)
years <- dimnames(ex_X)$y
n_years <- length(years)
ages <- c(4,5,6,7)
p_ages <- c(30,40,40,1)
n_ages <- length(ages)

# do the same with jacks included. Need for omega_J, for removing jacks from terminal mortalities.
ages_jacks <- c(3,4,5,6,7)
p_ages_jacks <- c(20,30,40,40,1)
n_ages_jacks <- length(ages_jacks)

# Do the same for Kitsumkalum, males and female separate
p_ages_males <- c(40,40,20,0)
p_ages_females <- c(0, 50, 50,0)

# Make up some age sample data
ad <- sapply(p_ages, FUN = function(x){ rpois( n = n_populations*n_years, lambda= x) })
ex_n <- array( ad,  dim = c(n_populations, n_years, n_ages), dimnames = list(i = populations, y = years, a = ages))

# Make up age sample data including 3 year old (jacks)
ad_jacks <- sapply(p_ages_jacks, FUN = function(x){ rpois( n = n_populations*n_years, lambda= x) })
ex_n_with_jacks <- array( ad_jacks,  dim = c(n_populations, n_years, n_ages_jacks), dimnames = list(i = populations, y = years, a = ages_jacks))

# Make up age sampling data for Kitsumkalum males and females
ad_males <- sapply(p_ages_males, FUN = function(x){ rpois( n = n_years, lambda= x) })
ex_n_males <- array( ad_males,  dim = c(n_years, n_ages), dimnames = list(y = years, a = ages))
ad_females <- sapply(p_ages_females, FUN = function(x){ rpois( n = n_years, lambda= x) })
ex_n_females <- array( ad_females,  dim = c(n_years, n_ages), dimnames = list(y = years, a = ages))


# Get age proportion data
omega <- get_omega(ex_n)
ex_omega <- omega$omega

# Get age proportion data with jacks
omega_J_all <- get_omega(ex_n_with_jacks)
ex_omega_J <- omega_J_all$omega["Skeena",,] # only include Skeena

# Get age proportion data for male and female Kitsumkalum escapement
get_omega_sex <- function(n) {
  sum_n_year <- apply(n, 1, sum)
  ages <- dimnames(n)$a
  years <- dimnames(n)$y
  # make array to fill in with age proportion values
  omega <- array( rep(NA, length(n)), dim=dim(n), dimnames = dimnames(n))
  for(a in ages) {
    for(y in years) {
      # divide number of aged fish of each age by total aged in each year.
      omega[ y,a ] <- n[ y,a] / sum_n_year[ y]
    }
  }
  return(omega)
}
ex_omega_KM <- get_omega_sex(ex_n_males)
ex_omega_KF <- get_omega_sex(ex_n_females)


# Make up data frame of terminal freshwater mortalities by type
ex_Tau <- data.frame( y = ex_k$year,
                      tyee = 500,
                      rec_catch_L = 500,
                      rec_release_L = 500,
                      FN_catch_L= 500,
                      rec_catch_U = 500,
                      FN_catch_U = 500)

# Terminal mortality upstream of Terrace data
ex_Tau_U_total <- get_Tau_U_total( omega_J = ex_omega_J, rec_catch_U = ex_Tau$rec_catch_U,
                                   FN_catch_U = ex_Tau$FN_catch_U)
# Optional: make up as fraction of return to Terrace.
#ex_Tau_U_total <- runif( dim(ex_X)[2], 0.1, 0.3) *   apply(ex_X[c("Middle Skeena", "Large Lakes", "Upper Skeena"), ], 2, sum)

# Get escapement
E <- get_E(K = ex_k$kitsumkalum_escapement, X = ex_X, Tau_U = ex_Tau_U_total,
   known_population = "Kitsumkalum",
    aggregate_population = "Skeena",
    lower_populations = c("Lower Skeena", "Zymoetz-Fiddler"),
    upper_populations = c("Upper Skeena", "Middle Skeena", "Large Lakes"))
ex_E <- E$E

# read in Kitsumkalum age-specific escapement
ksd <- read.csv(here("data-raw", "K_star.csv"))
names(ksd)[1] <- "y" # Reading in the first column name weird.
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

# Get wild spawners
W_star <- get_W_star(S_star = ex_S_star, H_star = ex_H_star,
    aggregate_population = "Skeena", hatchery_population = "Kitsumkalum")
ex_W_star <- W_star$W_star

#Get terminal mortalities freshwater
ex_Tau_L_total <- get_Tau_L_total( omega_J = ex_omega_J, tyee = ex_Tau$tyee,
                                   rec_catch_L = ex_Tau$rec_catch_L,
                                   rec_release_L = ex_Tau$rec_release_L,
                                   FN_catch_L = ex_Tau$FN_catch_L)

# lower
tau_L <- get_tau_L( Tau_L = ex_Tau_L_total, omega = ex_omega, P_tilde = ex_P_tilde, aggregate_population = "Skeena",
                    add_6_7 = TRUE)
ex_tau_L <- tau_L$tau_L

# Upper
# already made above
# ex_Tau_U_total <- get_Tau_U_total( omega_J = ex_omega_J, rec_catch_U = ex_Tau$rec_catch_U,
#                                    FN_catch_U = ex_Tau$FN_catch_U)

tau_U <- get_tau_U( Tau_U = ex_Tau_U_total, omega = ex_omega, P_tilde = ex_P_tilde,
    aggregate_population = "Skeena", upper_populations = c("Middle Skeena", "Large Lakes", "Upper Skeena"),
    lower_populations = c("Lower Skeena", "Kitsumkalum", "Zymoetz-Fiddler"),
    add_6_7 = TRUE)
ex_tau_U <- tau_U$tau_U

# Terminal marine mortality
use_arr <- ex_W_star["Kitsumkalum",,]
tau_dot_M <- array(runif(length(use_arr), 0.01, 0.2), dim = dim(use_arr), dimnames = dimnames(use_arr))
ex_tau_dot_M <- tau_dot_M
tau_M <- get_tau_M( W_star = ex_W_star, tau_dot_M = ex_tau_dot_M )
ex_tau_M <- tau_M$tau_M

# # total terminal mortality
tau <- get_tau_obsolete( tau_U = ex_tau_U, tau_L = ex_tau_L, tau_M = ex_tau_M)
ex_tau_obsolete <- tau$tau

# Proportion wild
p <- get_p(W_star = ex_W_star, E_star = ex_E_star, B_star = ex_B_star)
ex_p_wild <- p$p

# Wild terminal mortalities
tau_W <- get_tau_W(tau_U = ex_tau_U, tau_L = ex_tau_L, tau_M = ex_tau_M, p = ex_p_wild)
#tau_W <- get_tau_W_obsolete(tau = ex_tau, p = ex_p_wild)
ex_tau_W <- tau_W$tau_W

# Get terminal run
TermRun <- get_TermRun(tau_W = ex_tau_W, W_star = ex_W_star, B_star=ex_B_star)
ex_TermRun <- TermRun$TermRun

# Get mature run
use_arr <- ex_TermRun["Kitsumkalum",,]
phi_dot_M <- array(runif(length(use_arr), 0.01, 0.3), dim = dim(use_arr), dimnames = dimnames(use_arr))
ex_phi_dot_M <- phi_dot_M
MatureRun <- get_MatureRun(TermRun = ex_TermRun, phi_dot_M = ex_phi_dot_M)
ex_MatureRun <- MatureRun$MatureRun

# Preterminal post fishery abundance
# make up maturation rate data. Don't make for age 7
r <- array( runif(length(use_arr), 0.8, 0.9), dim = dim(use_arr),
    dimnames = dimnames(use_arr))
r[, "6" ] <- 1 # maturation rate = 1 for age 6 fish
ex_r <- r
A_phi <- get_A_phi(MatureRun = ex_MatureRun, r = ex_r)
ex_A_phi <- A_phi$A_phi

# Ocean pre-fishery abundance
phi_dot_E <- array(runif(length(use_arr), 0.01, 0.3), dim = dim(use_arr), dimnames = dimnames(use_arr))
ex_phi_dot_E <- phi_dot_E
A_P <- get_A_P( A_phi = ex_A_phi, phi_dot_E = ex_phi_dot_E)
ex_A_P <- A_P$A_P

# Preterminal fishing mortality in nominal fish
phi_N <- get_phi_N(A_P = ex_A_P, A_phi = ex_A_phi)
ex_phi_N <- phi_N$phi_N

# Preterminal fishing mortality in adult equivalents
use_arr <- ex_phi_N["Kitsumkalum",,]
# Make up some adult equivalents data
Q <- array(runif(length(use_arr), 0.5, 0.9), dim = dim(use_arr), dimnames = dimnames(use_arr))
Q[, "6" ] <- 1 # make maturation rate = 1 for age 6 fish
ex_Q <- Q
phi_Q <- get_phi_Q(phi_N = ex_phi_N, Q = ex_Q)
ex_phi_Q <- phi_Q$phi_Q

# Recruits
R_star <- get_R_star( MatureRun = ex_MatureRun, phi_Q = ex_phi_Q)
ex_R_star <- R_star

# Save example data sets to data/ as .rda files
usethis::use_data(ex_P, overwrite = TRUE)
usethis::use_data(ex_sigma_P, overwrite = TRUE)
usethis::use_data(ex_G, overwrite = TRUE)
usethis::use_data(ex_P_tilde, overwrite = TRUE)
usethis::use_data(ex_sigma_P_tilde, overwrite = TRUE)
usethis::use_data(ex_k, overwrite = TRUE)
usethis::use_data(ex_X, overwrite = TRUE)
usethis::use_data(ex_Tau_U_total, overwrite = TRUE)
usethis::use_data(ex_E, overwrite = TRUE)
usethis::use_data(ex_n, overwrite = TRUE)
usethis::use_data(ex_n_with_jacks, overwrite = TRUE)
usethis::use_data(ex_omega, overwrite = TRUE)
usethis::use_data(ex_omega_J, overwrite = TRUE)
usethis::use_data(ex_omega_KM, overwrite = TRUE)
usethis::use_data(ex_omega_KF, overwrite = TRUE)
usethis::use_data(ex_K_star, overwrite = TRUE)
usethis::use_data(ex_E_star, overwrite = TRUE)
usethis::use_data(ex_B_star, overwrite = TRUE)
usethis::use_data(ex_B, overwrite = TRUE)
usethis::use_data(ex_S_star, overwrite = TRUE)
usethis::use_data(ex_S, overwrite = TRUE)
usethis::use_data(ex_H_star, overwrite = TRUE)
usethis::use_data(ex_H, overwrite = TRUE)
usethis::use_data(ex_W_star, overwrite = TRUE)
usethis::use_data(ex_Tau_L_total, overwrite = TRUE)
usethis::use_data(ex_Tau, overwrite = TRUE)
usethis::use_data(ex_tau_L, overwrite = TRUE)
usethis::use_data(ex_Tau_U_total, overwrite = TRUE)
usethis::use_data(ex_tau_U, overwrite = TRUE)
usethis::use_data(ex_tau_dot_M, overwrite = TRUE)
usethis::use_data(ex_tau_M, overwrite = TRUE)
usethis::use_data(ex_tau_obsolete, overwrite = TRUE)
usethis::use_data(ex_p_wild, overwrite = TRUE)
usethis::use_data(ex_tau_W, overwrite = TRUE)
usethis::use_data(ex_TermRun, overwrite = TRUE)
usethis::use_data(ex_phi_dot_M, overwrite = TRUE)
usethis::use_data(ex_MatureRun, overwrite = TRUE)
usethis::use_data(ex_r, overwrite = TRUE)
usethis::use_data(ex_A_phi, overwrite = TRUE)
usethis::use_data(ex_phi_dot_E, overwrite = TRUE)
usethis::use_data(ex_A_P, overwrite = TRUE)
usethis::use_data(ex_phi_N, overwrite = TRUE)
usethis::use_data(ex_Q, overwrite = TRUE)
usethis::use_data(ex_phi_Q, overwrite = TRUE)
usethis::use_data(ex_R_star, overwrite = TRUE)

rm(list=ls()) # for testing examples

# ex_data <- grep("^ex_.*", names(.GlobalEnv), value=TRUE)
# ex_data_list <- do.call("list", mget(ex_data))
# lapply(ex_data_list, FUN = function(x) {usethis::use_data(x, overwrite = TRUE)})

