#' Get population proportions
#'
#' Do weekly expansions of catch by weekly genetic proportions, and then sum the weekly expansions by population and year
#'
#' @param P Numeric, array of genetic proportions with 3 dimensions: i (population), w (statistical week),and y (year)
#' @param sigma_P Numeric, array of SD of the genetic proportions, with 3 dimensions: i (population), w (week), and y (year).
#' @param G Numeric, array of how many Chinook were caught in the gillnet Tyee Test Fishery by week, with 2 dimensions: w (week) and y (year)
#' @param save_csv If TRUE, saves the first year of data as csv files for checking.
#'
#' @return List of 3 elements. First is an array of pooled genetic proportions with 2 dimensions: i (population) and y (year).
#' Second is an array of SD of pooled genetic proportions with 2 dimensions: i (population) and y (year). Third is a data frame
#' with P_tilde, sigma_P_tilde, and year merged for plotting and reporting.

#' @examples
#' library(abind)
#' library(here)
#' test <- make_P_G()
#' P <- test$P
#' sigma_P <- test$sigma_P
#' G <- test$G
#'
#' P_tilde <- get_P_tilde(P = P, sigma_P = sigma_P, G= G)
#'
#' @export
get_P_tilde <- function(P, sigma_P, G, save_csv = FALSE) {
  if(save_csv == TRUE) {
    write.csv( P[,,1], here("data-out/test-P.csv"))
    write.csv( sigma_P[,,1], here("data-out/test-sigma-P.csv"))
    write.csv( t(G[,1]), here("data-out/test-G.csv"))
  }
  populations <- dimnames(P)$i
  stopifnot( "Years are not equal for genetic and catch data" = all.equal(unique(dimnames(P)$y), unique(dimnames(G)$y)))
  years <- dimnames(P)$y
  n_years <- length(dimnames(P)$y)
  P_tilde_list <- list() # make empty list to store results for each year
  for(y in 1: n_years) { # cycle through years
    # multiply arrays to get weekly expansions. P is divided by 100 because it comes
    # from the Molecular Genetics Lab in proportions of 100.
    # Matrix multiplication, multiplies the proportion for each population and week by the catch for that week.
    # Then gets an annual sum for each population (sums rows, across weeks)
    # FLAG: this might need to be adjusted to remove non-summer run, upstream of Tyee populations.
    expand_weeks <- (P[ , , y] / 100) %*% G[ , y] # see https://mbernste.github.io/posts/matrix_vector_mult/
    # Make into proportion of yearly catch by dividing expansions for each population
    # by the total yearly catch.
    P_tilde_list[[y]] <- expand_weeks / sum(G[ , y]) # test that these all add to 1 within each year.
  }
  P_tilde <- abind( P_tilde_list, along=2) # bind list into array
  dimnames(P_tilde) <- list(i = populations, y = years) # fix names of dimensions

  # sigma for P_tilde
  sigma_P_tilde_list <- list() # make empty list to store results for each year
  for(y in 1: n_years) { # cycle through years
    # multiply arrays to get weekly expansions. sigma_P is divided by 100 because it comes
    # from the Molecular Genetics Lab in proportions of 100.
    # FLAG: this might need to be adjsuted to remove non-summer run, upstream of Tyee populations.
    expand_weeks <- t(t((sigma_P[,,y] / 100)) * G[,y]) # Have to transpose to get rows and columns of results right. Checked by hand in excel.
    sum_sd <- apply(expand_weeks, 1, function(y) { sqrt(sum(y^2)) }) # Add SD by squaring, adding, then square root.
    # Make into proportion of yearly catch by dividing expansions for each population
    # by the total yearly catch.
    sigma_P_tilde_list[[y]] <- sum_sd / sum(G[ ,y]) # Divide by total catch for year
  }
  sigma_P_tilde <- abind( sigma_P_tilde_list, along=2) # bind list into array
  dimnames(sigma_P_tilde) <- list(i = populations, y = years) # fix names of dimensions
# add the data frame merged
  d <- as.data.frame.table(P_tilde, responseName = "P_tilde", stringsAsFactors = FALSE)
  ds <- as.data.frame.table(sigma_P_tilde, responseName = "sigma_P_tilde", stringsAsFactors = FALSE)
  dt <- merge(d, ds, by = c("i", "y"))
  dt$y <- as.integer(dt$y)

  res <- list(P_tilde = P_tilde, sigma_P_tilde = sigma_P_tilde, df  = d)
  res
}

