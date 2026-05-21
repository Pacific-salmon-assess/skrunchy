#' Do run reconstruction with uncertainty, iterate many times
#'
#' See [rru()] for a single run.
#'
#' @param n_iter Inter, number of iterations to run. Defaults to 1000.
#' @param seed Integer or NULL, should a seed be set? For reproducibiliy. Default to 1.
#' @inheritDotParams rru
#'
#' @returns
#'
#' List of results.
#'
#'
#' @export
#'
#' @examples
#'  n_gsi_samples <- apply(ex_n, 2, sum)
#'  n_age_samples <- apply(ex_n, c(1,2), sum)
#'  n_age_samples_J <- apply(ex_n_with_jacks, c(1,2), sum)
#'  results <- rru_iterate(
#'    n_iter = 10
#'    P = ex_P,
#'    sigma_P = ex_sigma_P,
#'    G = ex_G,
#'    K = ex_k$kitsumkalum_escapement,
#'    n_gsi_samples = n_gsi_samples,
#'    n_age_samples = n_age_samples,
#'    n_age_samples_J = n_age_samples_J,
#'    sigma_K = ex_k$sd,
#'    y_K = ex_k$year,
#'    omega = ex_omega,
#'    omega_J = ex_omega_J,
#'    tyee = ex_Tau$tyee,
#'    rec_catch_L = ex_Tau$rec_catch_L,
#'    rec_release_L = ex_Tau$rec_release_L,
#'    FN_catch_L = ex_Tau$FN_catch_L,
#'    rec_catch_U = ex_Tau$rec_catch_U,
#'    FN_catch_U = ex_Tau$FN_catch_U,
#'    known_population = "Kitsumkalum",
#'    aggregate_population = "Skeena",
#'    lower_populations = c("Lower Skeena", "Zymoetz"),
#'    upper_populations = c("Upper Skeena", "Middle Skeena", "Large Lakes"),
#'    K_star = ex_K_star,
#'    add_6_7 = TRUE,
#'    B_star = ex_B_star,
#'    H_star = ex_H_star,
#'    tau_dot_M = ex_tau_dot_M,
#'    phi_dot_M = ex_phi_dot_M,
#'    r = ex_r,
#'    phi_dot_E = ex_phi_dot_E,
#'    Q = ex_Q,
#'    name_key = variable_name_key,
#'    save_outputs = FALSE,
#'    )
#'
rru_iterate <- function(
  n_iter = 1000,
  seed = 1,
  ...
) {
  j <- n_iter
  results <- vector("list", j)

  for (i in seq_len(j)) {
    if (!is.null(seed)) {
      set.seed(seed + i)
    }

    results[[i]] <- rru(..., iteration_number = i)
  }

  results
}
