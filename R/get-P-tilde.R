#' Get population proportions
#'
#' Do weekly expansions of catch by weekly genetic proportions, and then sum the weekly expansions by population and year
#'
#' @param P Numeric, array of genetic proportions with 3 dimensions: i (population), w (statistical week),and y (year)
#' @param sigma_P Numeric, array of SD of the genetic proportions, with 3 dimensions: i (population), w (week), and y (year).
#' @param G Numeric, array of how many Chinook were caught in the gillnet Tyee Test Fishery by week, with 2 dimensions: w (week) and y (year)
#' @param save_csv If TRUE, saves the first year of data as csv files for checking.
#' @param save_location Sub-directory folder of where to save csv
#' @param save_name File name of csv to save. Defaults to "P_tilde.csv"
#'
#' @return List of 3 elements. First is an array of pooled genetic proportions with 2 dimensions: i (population) and y (year).
#' Second is an array of SD of pooled genetic proportions with 2 dimensions: i (population) and y (year). Third is a data frame
#' with P_tilde, sigma_P_tilde, and year merged for plotting and reporting.

#' @examples
#' P_tilde <- get_P_tilde( P = ex_P, sigma_P = ex_sigma_P, G = ex_G )
#'
#' @export
get_P_tilde <- function(P, sigma_P, G, save_csv = FALSE, save_location,
                        save_name = "P_tilde.csv") {
  # Check populations, weeks, and years of input data are the same
  # populations (P and sigma_P only)
  if(!dim(P)[1] == dim(sigma_P)[1] )  {
    stop("Length of population (i) dimensions not equal.") }
  if(!all(dimnames(P)$i %in% dimnames(sigma_P)$i)) {
    stop("Population (i) values are not equal.")    }
  # weeks
  if(!all.equal( dim(P)[2], dim(sigma_P)[2] , dim(G)[1]))  {
    stop("Length of week (w) dimensions not equal.") }
   # Flag: not checking names of weeks, they may be in stat weeks or start at 1
  # if(!all(dimnames(P)$w %in% dimnames(sigma_P)$w , dimnames(P)$w %in% dimnames(G)$w )) {
  #   stop("Week (w) values are not equal.")    }
  # years
  if(!all.equal( dim(P)[3], dim(sigma_P)[3] , dim(G)[2]))  {
    stop("Length of year (y) dimensions not equal.") }
  if(!all( dimnames(P)$y %in% dimnames(sigma_P)$y , dimnames(P)$y %in% dimnames(G)$y )) {
    stop("Year (y) values are not equal.")    }

  populations <- dimnames(P)$i
  years <- dimnames(P)$y

  P_tilde_list <- list() # make empty list to store results for each year
  for(y in years) { # cycle through years
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
  for(y in years) { # cycle through years
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

  if(save_csv == TRUE) {
    save_to <- here(save_location, save_name )
    write.csv(dt, save_to, row.names = FALSE)
  }
  res <- list(P_tilde = P_tilde, sigma_P_tilde = sigma_P_tilde, df  = dt)
  res
}

