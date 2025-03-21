#' Example data of terminal marine exploitation rates
#'
#' Array of terminal marine exploitation rates with dimensions year (y) and age (a). Sum of exploitation rate for terminal
#' marine net fisheries and exploitation rate for terminal marine sport fisheries
#' (coded "TNBC TERM N" and "TNBC TERM S" by the Chinook Technical Committee, respectively), which are outputs from the CTC Chinook model / Kitsumkalum CWT analysis.
#'
#' @format ## `ex_tau_dot_M`
#' An array with two dimensions:
#' \describe{
#'   \item{y}{Year}
#'   \item{a}{Age}
#' }
#' @source data-raw/make-example-data.R
"ex_tau_dot_M"
