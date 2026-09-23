#' Do run reconstruction with uncertainty, iterate many times
#'
#' See [rru()] for a single run.
#'
#' @param n_iter Inter, number of iterations to run. Defaults to 1000.
#' @param seed Integer or NULL, should a seed be set? For reproducibiliy. Default to 1.
#' @inheritDotParams rru
#' @inheritParams rru
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
#'    n_iter = 10,
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
#'    use_tyee= FALSE,
#'    cv_freshwater_mortality = 0.3,
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
#'    rate_resample_method = "lognormal",
#'    rate_resample_cv = 0.3,
#'    rate_resample_max_er = 0.5,
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
  x <- results
  dat <- dplyr::bind_rows(x) |>
    dplyr::ungroup()

  sdat <- dat |>
    dplyr::group_by(i_population, y_return_year) |>
    dplyr::summarise(
      dplyr::across(
        c("W_wild_spawners", "harvest", "N_total_run", "est_hr"),
        # Add 5th and 95th percentiles to summary stats
        list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE)),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(
      cols = -c(i_population, y_return_year),
      names_to = c("variable", ".value"),
      names_pattern = "^(.*)_(mean|sd)$"
    )

  # list of arrays, one for each variable
  # Values and labels for each array dimension
  dim_levels <- list(
    i_population = unique(dat$i_population),
    y_return_year = sort(unique(dat$y_return_year)),
    iter_n = sort(unique(dat$iter_n))
  )

  # Row-level array indices
  idx <- cbind(
    match(dat$i_population, dim_levels$i_population),
    match(dat$y_return_year, dim_levels$y_return_year),
    match(dat$iter_n, dim_levels$iter_n)
  )

  # Convert columns 3:6 into separate arrays
  array_list <- setNames(
    lapply(names(dat)[3:6], function(var) {
      out <- array(
        NA_real_,
        dim = lengths(dim_levels),
        dimnames = dim_levels
      )
      out[idx] <- dat[[var]]
      out
    }),
    names(dat)[3:6]
  )

  full_results_list <- list(results, sdat, array_list)
}
