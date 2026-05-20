#' Do run reconstruction, conventional based on Winther et al. 2024
#'
#' Wrapper function that does the whole run reconstruction, option to save the outputs
#' into data/ folder as .rds files.
#'
#' @inheritParams get_P_tilde
#' @inheritParams get_X
#' @inheritParams get_Tau_L_total
#' @inheritParams get_Tau_U_total
#' @inheritParams get_E
#' @inheritParams get_E_star
#' @inheritParams get_S_star
#' @inheritParams get_W_star
#' @inheritParams get_p
#' @inheritParams get_tau_L
#' @inheritParams get_tau_U
#' @inheritParams get_tau_M
#' @inheritParams get_TermRun
#' @inheritParams get_MatureRun
#' @inheritParams get_A_phi
#' @inheritParams get_A_P
#' @inheritParams get_phi_N
#' @inheritParams get_phi_Q
#' @inheritParams get_N
#' @inheritParams get_R
#' @param name_key Dataframe, with details on names
#' @param save_outputs Logical, whether to save the outputs. Default is TRUE.
#'
#' @returns
#' List of results.
#' If save_outputs == TRUE, then it also saves
#'
#'
#' @export
#'
#' @examples
#'
#'  rr <- function(
#'    P = ex_P,
#'    sigma_P = ex_sigma_P,
#'    G = ex_G,
#'    K = ex_k$kitsumkalum_escapament,
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
#'    save_outputs = FALSE
#'    )
#'
#'
#'
#'
rr <- function(
  P,
  sigma_P,
  G,
  K,
  sigma_K,
  y_K,
  omega,
  omega_J,
  tyee,
  rec_catch_L,
  rec_release_L,
  FN_catch_L,
  rec_catch_U,
  FN_catch_U,
  known_population = "Kitsumkalum",
  aggregate_population = "Skeena",
  lower_populations = c("Lower Skeena", "Zymoetz"),
  upper_populations = c("Upper Skeena", "Middle Skeena", "Large Lakes"),
  K_star,
  add_6_7 = TRUE,
  B_star,
  H_star,
  tau_dot_M,
  phi_dot_M,
  r,
  phi_dot_E,
  Q,
  name_key,
  save_outputs = TRUE
) {
  # # Get number of GSI samples in each year
  # n_gsi_samples <- as.numeric(table(tyee_biodata_age_sex_length_CU$y))
  # P_tilde$P_tilde
  # dim(P_tilde$P_tilde)
  # n_years <- dim(P_tilde$P_tilde)[2]
  # P_tilde_sample <- P_tilde$P_tilde
  # for (i in 1:n_years) {
  #   P_tilde_sample[, i] <- rmultinom(
  #     n = 1,
  #     size = n_gsi_samples[i],
  #     prob = P_tilde$P_tilde[, i]
  #   ) /
  #     n_gsi_samples[i]
  # }

  # Use data from Skeena Tyee test fishery weekly catch and genetic mixture data,
  # and pool it into annual genetic proportions.
  P_tilde <- get_P_tilde(P = P, sigma_P = sigma_P, G = G)
  # Now do expansions to get returns to Terrace for each population, and the
  # Skeena aggregate.
  X <- get_X(
    P_tilde = P_tilde$P_tilde,
    sigma_P_tilde = P_tilde$sigma_P_tilde,
    K = K,
    sigma_K = sigma_K,
    y_K = y_K
  )
  # Estimate terminal mortalities, total by year
  # Get freshwater terminal mortalities in the lower Skeena by year
  Tau_L_total <- get_Tau_L_total(
    omega_J = omega_J,
    tyee = tyee,
    rec_catch_L = rec_catch_L,
    rec_release_L = rec_release_L,
    FN_catch_L = FN_catch_L
  )
  # Get freshwater terminal mortalities in the upper Skeena by year
  Tau_U_total <- get_Tau_U_total(
    omega_J = omega_J,
    rec_catch_U = rec_catch_U,
    FN_catch_U = FN_catch_U
  )
  # Get escapement for each population, plot with returns to Terrace (note, will
  # only be different for Skeena aggregate and the three upper populations).
  # See below for $T_U$ calculations.
  E <- get_E(
    K = K,
    X = X$X,
    Tau_U = Tau_U_total,
    known_population = "Kitsumkalum",
    aggregate_population = "Skeena",
    lower_populations = c("Lower Skeena", "Zymoetz"),
    upper_populations = c("Upper Skeena", "Middle Skeena", "Large Lakes")
  )
  # Get age proportions by age, population and year
  omega
  # Get age-specific escapement for Kitsumkalum River, from sex-specific age
  # proportions, sex-specific escapement, and hatchery contributions.
  K_star
  # Get age-specific escapement by using age proportions.
  E_star <- get_E_star(
    E = E$E,
    omega = omega,
    K_star = K_star,
    add_6_7 = TRUE
  )
  # Get spawners for each population (accounts for brood stock removals).
  # Spawners should only be different from escapement for Skeena aggregate and
  # Kitsumkalum, since brood removals are only for Kitsumkalum.
  S_star <- get_S_star(E_star = E_star$E_star, B_star = B_star)
  # Get wild spawners for each population (accounts for hatchery origin spawners),
  # Wild spawners should only be different from spawners for Skeena aggregate and
  # Kitsumkalum, since hatchery origin spawners only occur for Kitsumkalum.
  W_star <- get_W_star(S_star = S_star$S_star, H_star = H_star)
  # Get proportion wild spawners for each population. Should only be <1
  # for Skeena aggregate and Kitsumkalum, since hatchery origin spawners only occur
  # for Kitsumkalum. Note this is not real data, p is very high for Kitsumkalum across years.
  p <- get_p(W_star = W_star$W_star, E_star = E_star$E_star, B_star = B_star)
  p_proportion_wild <- p # For saving, otherwise R gets confused between P and p
  # Estimate freshwater terminal mortalities in the lower Skeena by population, year, and age.
  tau_L <- get_tau_L(
    Tau_L = Tau_L_total,
    omega = omega,
    P_tilde = P_tilde$P_tilde,
    aggregate_population = "Skeena",
    add_6_7 = TRUE
  )
  # Estimate freshwater terminal mortalities in the upper Skeena by population,
  # year, and age. Should be 0 for Kitsumkalum, Lower Skeena, and Zymoetz-Fiddler.
  tau_U <- get_tau_U(
    Tau_U = Tau_U_total,
    omega = omega,
    P_tilde = P_tilde$P_tilde,
    aggregate_population = "Skeena",
    upper_populations = c("Middle Skeena", "Large Lakes", "Upper Skeena"),
    lower_populations = c("Lower Skeena", "Kitsumkalum", "Zymoetz"),
    add_6_7 = TRUE
  )
  # Estimate marine terminal mortalities in the marine area by population, year, and age.
  tau_M <- get_tau_M(W_star = W_star$W_star, tau_dot_M = tau_dot_M)
  # Get wild total terminal mortality
  tau_W <- get_tau_W(
    tau_U = tau_U$tau_U,
    tau_L = tau_L$tau_L,
    tau_M = tau_M$tau_M,
    p = p$p
  )
  # Get wild terminal run
  TermRun <- get_TermRun(
    tau_W = tau_W$tau_W,
    W_star = W_star$W_star,
    B_star = B_star
  )
  # Get mature run
  MatureRun <- get_MatureRun(TermRun = TermRun$TermRun, phi_dot_M = phi_dot_M)
  # Get pre-terminal post fishery abundance.
  A_phi <- get_A_phi(MatureRun = MatureRun$MatureRun, r = r)
  # Get pre-fishery ocean abundance.
  A_P <- get_A_P(A_phi = A_phi$A_phi, phi_dot_E = phi_dot_E)
  # Get preterminal fishing mortality in nominal fish.
  phi_N <- get_phi_N(A_P = A_P$A_P, A_phi = A_phi$A_phi)
  # Get preterminal fishing mortality in adult equivalents.
  phi_Q <- get_phi_Q(phi_N = phi_N$phi_N, Q = Q)
  # Get total run
  N <- get_N(MatureRun = MatureRun$MatureRun, phi_Q = phi_Q$phi_Q)
  # Get recruits by return year and brood year.
  R <- get_R(N = N$N)

  # 4. Process and merge run reconstruction data into tables --------------------

  # Merge all data frames by the return year, age, and CU

  # Make a data frame with brood stock removals, equal for Skeena and Kitsumkalum, 0 for all other CUs
  B_star_df <- array2DF(B_star, responseName = "B_star")

  brood_pops <- c("Kitsumkalum", "Skeena")
  CU_dummy <- expand.grid(brood_pops, unique(B_star_df$y), unique(B_star_df$a))
  names(CU_dummy) <- c("i", "y", "a")
  CU_dummy2 <- expand.grid(
    unique(X$df$i)[!unique(X$df$i) %in% brood_pops],
    unique(B_star_df$y),
    unique(B_star_df$a)
  )
  names(CU_dummy2) <- c("i", "y", "a")
  CU_dummy2$B_star <- 0

  B_star_df1 <- merge(B_star_df, CU_dummy, by = c("y", "a"), all = TRUE)
  B_star_df2 <- rbind(B_star_df1, CU_dummy2)
  # ggplot( B_star_df2, aes ( y = B_star, x  = y)) +
  #   geom_line()+ geom_point() +
  #   facet_grid( i ~ a) +
  #   theme_classic()

  # same for H_star
  H_star_df <- array2DF(H_star, responseName = "H_star")

  brood_pops <- c("Kitsumkalum", "Skeena")
  CU_dummy <- expand.grid(brood_pops, unique(H_star_df$y), unique(H_star_df$a))
  names(CU_dummy) <- c("i", "y", "a")
  CU_dummy2 <- expand.grid(
    unique(X$df$i)[!unique(X$df$i) %in% brood_pops],
    unique(H_star_df$y),
    unique(H_star_df$a)
  )
  names(CU_dummy2) <- c("i", "y", "a")
  CU_dummy2$H_star <- 0

  H_star_df1 <- merge(H_star_df, CU_dummy, by = c("y", "a"), all = TRUE)
  H_star_df2 <- rbind(H_star_df1, CU_dummy2)
  # ggplot( H_star_df2, aes ( y = H_star, x  = y)) +
  #   geom_line()+ geom_point() +
  #   facet_grid( i ~ a) +
  #   theme_classic()

  # List of data with CU and return year and age
  list_df_iya <- list(
    # omega$df,
    #"n" = n # n age observations, would need to make into df
    E_star$df,
    B_star_df2,
    S_star$df,
    H_star_df2,
    W_star$df,
    p$df,
    tau_L$df,
    tau_U$df,
    tau_M$df,
    tau_W$df,
    TermRun$df,
    MatureRun$df,
    A_phi$df,
    A_P$df,
    phi_N$df,
    phi_Q$df,
    N$df
  )

  dc <- combine_df_list(list_df_iya, includes_ages = TRUE)

  # list of data by CU and return year, no ages
  list_df_iy <- list(P_tilde$df, X$df, E$df)
  dciy <- combine_df_list(list_df_iy, includes_ages = FALSE)

  # Get total harvest estimate
  # Total (wild) run minus wild spawners minus brood removals
  dc$total_harvest_estimate <- dc$N - dc$W_star - dc$B_star

  # merge ERA rates into data frame for merging
  listarr <- list(
    "H_star" = H_star,
    "tau_dot_M" = tau_dot_M,
    "phi_dot_M" = phi_dot_M,
    "r" = r,
    "phi_dot_E" = phi_dot_E,
    "Q" = Q
  )
  ERA_w <- arrays_to_df(listarr)
  ERA_l <- ERA_w |>
    tidyr::pivot_longer(cols = 3:8, names_to = "var", values_to = "value")
  ERA_data_processed <- list(df_wide = ERA_w, df_long = ERA_l)

  # merge ERA rates with run recon table

  dc <- merge(
    dc,
    ERA_data_processed$df_wide[
      !grepl("H_star", names(ERA_data_processed$df_wide))
    ],
    by = c("y", "a", "b"),
    all.x = TRUE
  )

  #variable_name_key

  new_names <- names(dc)
  for (i in 1:length(names(dc))) {
    new_names[i] <- ifelse(
      names(dc)[i] %in% name_key$skrunchy,
      name_key$skrunchy_with_detail[
        name_key$skrunchy == names(dc)[i]
      ],
      names(dc)[i]
    )
  }
  #new_names

  dcn <- dc
  names(dcn) <- new_names

  run_reconstruction_table <- dcn

  # Sum wild spawners, total harvest, and total run by CU and return year
  dcsum <- dc |>
    dplyr::filter(.data$a != 7) |>
    dplyr::group_by(.data$i, .data$y) |>
    dplyr::summarize(
      W_wild_spawners = sum(.data$W_star, na.rm = TRUE),
      harvest = sum(.data$total_harvest_estimate, na.rm = TRUE),
      N = sum(.data$N, na.rm = TRUE)
    )
  dcsum1 <- dcsum
  dcsum1$est_hr <- dcsum1$harvest / dcsum$N

  new_names1 <- names(dcsum1)
  for (i in 1:length(names(dcsum1))) {
    new_names1[i] <- ifelse(
      names(dcsum1)[i] %in% name_key$skrunchy,
      name_key$skrunchy_with_detail[
        name_key$skrunchy == names(dcsum1)[i]
      ],
      names(dcsum1)[i]
    )
  }
  new_names1
  names(dcsum1) <- new_names1
  run_reconstruction_table_summary <- dcsum1

  # columns dcsum1# columns to merge into brood table
  btmc <- c("i", "y", "W_wild_spawners")
  # Make brood table
  brood_table <- merge(
    R$df,
    dcsum[, names(dcsum) %in% btmc],
    by.x = c("b", "i"),
    by.y = c("y", "i")
  )

  if (save_outputs == TRUE) {
    # 5. Save data -----------------------
    # save the step by step data products
    usethis::use_data(
      P_tilde,
      X,
      Tau_L_total,
      Tau_U_total,
      E,
      E_star,
      S_star,
      W_star,
      p_proportion_wild,
      tau_L,
      tau_U,
      tau_M,
      tau_W,
      TermRun,
      MatureRun,
      A_phi,
      A_P,
      phi_N,
      phi_Q,
      N,
      R,
      overwrite = TRUE
    )

    # Save summarized/ merged data objects
    usethis::use_data(
      run_reconstruction_table,
      run_reconstruction_table_summary,
      brood_table,
      overwrite = TRUE
    )

    # Save big table to csv
    write.csv(
      run_reconstruction_table,
      here("data", "run_reconstruction_table.csv"),
      row.names = FALSE
    )
  }

  results <- list(
    run_reconstruction_table,
    run_reconstruction_table_summary,
    brood_table
  )
  results
}
