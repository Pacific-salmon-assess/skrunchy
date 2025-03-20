#' Example preterminal post-fishery abundance for testing and examples
#'
#' Preterminal post-fishery abundance, which is the abundance of the
#' cohort after non-net fisheries (e.g., troll), after accounting for maturation rate but
#' before preterminal net (gillnet, seine) fishery harvest of mature fish. Of the fish that survived
#' non-net ocean fisheries, the number of fish that matured into the mature run.
#'
#'
#' @format ## `ex_A_phi`
#' An array with three dimensions:
#' \describe{
#'   \item{i}{Population}
#'   \item{y}{Year}
#'   \item{a}{Age}
#' }
#' @source data-raw/make-example-data.R, get_A_phi()
"ex_A_phi"
