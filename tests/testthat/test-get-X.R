test_that("Populations sum to aggregate", {
  d <- make_P_G(start_year = 2000, end_year = 2001)
  res <- get_P_tilde(P = d$P, sigma_P = d$sigma_P, G = d$G)
  k <- data.frame( year = c(2000, 2001),
                  kitsumkalum_escapement = c(5000, 6000),
                  sd = c(500, 400))
  known_population <-  "Kitsumkalum"
  aggregate_population <- "Skeena"
  X <- get_X(P_tilde = res$P_tilde, sigma_P_tilde = res$sigma_P_tilde, K= k$kitsumkalum_escapement,
             sigma_K = k$sd,
             y = k$year, known_population = known_population)
  not_aggregate <- dimnames(X$X)$i[ !dimnames(X$X)$i %in% aggregate_population]
  expect_equal( sum( X$X[ not_aggregate, 1] )  ,  X$X[aggregate_population, 1]  )
})
