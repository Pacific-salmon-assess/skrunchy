#' Get total age from scale and Coded Wire Tag (CWT) ages
#'
#' When there are CWT ages and scale ages, uses CWT age if they are different.
#'
#' @param scale_age Numeric or integer, age from scales. Gilbert-Rich age (52 = total age of 5, etc.)
#' @param cwt_age Numeric or integer, age from Coded Wire Tag (CWT), equal to recovery year - brood year
#'
#' @returns Integer, total age
#' @export
#'
#' @examples
#'
#' scale_age <- c(4,5,6,NA, 4, NA)
#' cwt_age <- c( NA, 4, 5, 5, NA, NA)
#' a <- get_correct_age( scale_age = scale_age, cwt_age = cwt_age )
#'
get_correct_age <- function(scale_age, cwt_age) {
  if (!length(scale_age) == length(cwt_age)) {
    stop(
      "Number of scale and CWT age observations are not equal. Check length of data inputs."
    )
  }
  correct_age <- as.integer(rep(NA, length = length(scale_age)))

  for (i in 1:length(correct_age)) {
    if (is.na(scale_age[i])) {
      # if there is no scale age, use cwt age
      correct_age[i] <- cwt_age[i]
    }

    if (is.na(cwt_age[i])) {
      # if there is no cwt age, use scale age
      correct_age[i] <- scale_age[i]
    }

    if (!is.na(scale_age[i]) && !is.na(cwt_age[i])) {
      # if there is both scale and cwt ages, use cwt age
      correct_age[i] <- cwt_age[i]
    }
  }

  as.integer(correct_age)
}
