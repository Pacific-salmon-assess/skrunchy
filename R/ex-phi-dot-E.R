#' Example data of preterminal total mortality exploitation rates
#'
#' Array of preterminal total mortality exploitation rates
#' for non-net fisheries (e.g., troll), with two dimensions: year (y) and age (a).
#' From the CTC Chinook model output for Kitsumkalum CWT releases (KLM).
#' Equal to the number of fish caught plus incidental mortalities for non-net fisheries (e.g., troll), in
#' areas seaward of the terminal area, divided by total stock for the cohort, in nominal fish (not adjusted for AEQs).
#'
#' @format ## `ex_phi_dot_E`
#' An array with two dimensions:
#' \describe{
#'   \item{y}{Year}
#'   \item{a}{Age}
#' }
#' @source data-raw/make-example-data.R
"ex_phi_dot_E"
