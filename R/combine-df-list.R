#' Combine data frames of results
#'
#' Combine data results from run reconstruction, merges by return year, population, and (if present) age.
#'
#' @param x List of data frames to merge.
#' @param includes_ages Logical, do data frames include an age column to merge by?
#'
#' @returns Data frame of merged results.
#'
#'
#' @export
#'
#' @examples
#' S_star1 <- get_S_star(E_star = ex_E_star, B_star = ex_B_star)
#' W_star1 <- get_W_star( S_star = S_star1$S_star, H_star = ex_H_star)
#' df_list <- list( S_star1$df, W_star1$df)
#' combine_df_list( x = df_list )
#'
#'
combine_df_list <- function(x, includes_ages = TRUE) {
  # remove brood year column before merging
  dn <- lapply(x, function(i) {
    df_new <- i[, !grepl("^b$", names(i))]
    df_new
  })

  if (includes_ages == TRUE) {
    combdf <- Reduce(
      function(d1, d2) {
        merge(d1, d2, by = c("y", "a", "i"), all = TRUE)
      },
      dn
    )
    combdf$a <- as.integer(combdf$a)
    combdf$y <- as.integer(combdf$y)
    combdf$b <- combdf$y - combdf$a
  }

  if (includes_ages == FALSE) {
    combdf <- Reduce(
      function(d1, d2) {
        merge(d1, d2, by = c("y", "i"), all = TRUE)
      },
      dn
    )
  }

  return(combdf)
}
