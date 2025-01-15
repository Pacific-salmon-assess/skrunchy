test_that("Total mortalities by year populations sum to aggregate population", {
  populations <- c("Kitsumkalum", "Lower Skeena", "Zymoetz-Fiddler", "Upper Skeena", "Middle Skeena", "Large Lakes", "Skeena")
  n_populations <- length(populations)
  years <- 2000:2001
  n_years <- length(years)
  ages <- c(3,4,5,6,7)
  p_ages <- c(20,30,40,40,1)
  n_ages <- length(ages)
  d <- sapply(p_ages, FUN = function(x){ rpois( n = n_populations*n_years, lambda= x) })
  n <- array( d,  dim = c(n_populations, n_years, n_ages), dimnames = list(i = populations, y = years, a = ages))
  omega <- get_omega(n)
  Tau_U <- sample(500:1000, size=n_years)
  P_G <-  make_P_G( start_year = 2000, n_years = n_years, population_names = populations[1:6])
  P_tilde <- get_P_tilde( P = P_G$P, sigma_P = P_G$sigma_P, G = P_G$G)
  tau_U <- get_tau_U( Tau_U = Tau_U, omega = omega$omega, P_tilde = P_tilde$P_tilde, aggregate_population = "Skeena",
                      upper_populations = c("Middle Skeena", "Large Lakes", "Upper Skeena"),
                      lower_populations = c("Lower Skeena", "Kitsumkalum", "Zymoetz-Fiddler") , add_6_7 = FALSE)
  expect_equal( apply(tau_U$tau_U["Skeena",,],1,sum),    apply( tau_U$tau_U[ setdiff(populations, "Skeena") ,,], 2,sum))
  expect_equal( as.numeric(apply(tau_U$tau_U["Skeena",,],1,sum)), Tau_U )
})
