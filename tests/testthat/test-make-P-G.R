test_that("P and sigma_P have equal dimension names", {
  a <- make_P_G()
  expect_equal(dimnames(a$P), dimnames(a$sigma_P))
})
