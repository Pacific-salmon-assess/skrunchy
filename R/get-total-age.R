#' Get total Gilbert-Rich age
#'
#' Uses the Gilbert-Rich age from the Scale age lab, and returns the total age (integer).
#' Partial ages from regrowth scales (e.g., 3M, 2M) become NAs.
#'
#'
#' @param GR_age Character, vector of Gilbert-Rich ages as they come from the SCL (sclerochronology lab).
#'
#' @returns Integer, vector of total age. For example. a 52 fish becomes 5.
#'
#' @examples
#'
#' raw_ages <- c("32", "42", "52", "51", "62", "2M", "RG", "1M")
#' age <- get_total_age(raw_ages)
#'
#' @export
get_total_age <- function(GR_age) {
  as.integer(ifelse( grepl( "[[:digit:]]{2}", GR_age), substr(GR_age, 1,1) , NA))
}

