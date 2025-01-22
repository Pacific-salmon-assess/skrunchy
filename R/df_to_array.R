#' Convert a data frame into an array.
#'
#' Takes a data frame and makes an array using a chosen variable as the array value, and other variables
#' from the data frame as dimensions. Variable names are used as the dimension names of the new array.
#'
#' @param df Dataframe input.
#' @param value Name of the dataframe variable to use for the array value.
#' @param dimnames_order Variable names to use for dimensions of the new array (in the order you want them to be in the array)
#' @param FUN Optional, function to use if there are more than 1 value for each combination of dimensions. Default is print().
#' @param default Value to use if there are missing values for combinations of dimension. Default is 0.
#'
#' @return An array with values equal to the dataframe value column, dimensions and dimension names as supplied.
#'
#' @examples
#' i <- c(rep("A", 4), rep("B", 9), rep("C", 16))
#' y <- c(rep(2000:2001, each = 2), rep(2000:2002, each = 3), rep(2000:2003, each=4))
#' w <- c(rep(1:2, times = 2), rep(1:3, times = 3), rep(1:4, times = 4))
#' p <- runif(length(i), 0,1)
#' d <- data.frame(i, y, w, p)
#' arr <- df_to_array( df = d, value = "p", dimnames_order = c("i", "y", "w"))

#' @export
df_to_array <- function( df, value, dimnames_order, FUN = print, default = 0) {
  dim_position <- sapply(dimnames_order, function(x) grep( paste0("^", x, "$"), names(df)) )
  array <- tapply( df[ ,value], df[ ,dim_position], FUN = FUN, default = default)
  return(array)
}
