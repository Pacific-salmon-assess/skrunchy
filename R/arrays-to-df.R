#' Combine arrays into data frame
#'
#' @param x list of arrays to combine
#' @param dimnames_merge Character, vector of names of dimensions to merge by
#'
#' @returns
#' Merged data frame
#'
#' @export
#'
#' @examples
#' listarr <- list(
#'   "H_star" = ex_H_star,
#'   "tau_dot_M" = ex_tau_dot_M,
#'   "phi_dot_M" = ex_phi_dot_M,
#'   "r" = ex_r,
#'   "phi_dot_E" = ex_phi_dot_E,
#'   "Q" = ex_Q
#'     )
#'  ERA_w <- arrays_to_df(listarr)
#'
#'
#'
#'
arrays_to_df <- function(x, dimnames_merge = c("y", "a")) {
  # Check list has names
  if (is.null(names(x))) {
    stop("List must be named.")
  }

  # Convert each array to data frame using its list name
  l_df <- Map(
    function(arr, nm) {
      array2DF(arr, responseName = nm)
    },
    x,
    names(x)
  )

  # Merge all data frames by the first two dimension columns
  combdf <- Reduce(
    function(d1, d2) {
      merge(d1, d2, by = dimnames_merge, all = TRUE)
    },
    l_df
  )

  combdf$y <- as.numeric(combdf$y)
  combdf$a <- as.numeric(combdf$a)
  combdf$b <- combdf$y - combdf$a
  return(combdf)
}
