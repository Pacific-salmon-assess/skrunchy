#' Example recruits by population, return year, brood year, and age for testing and examples
#'
#' List with three elements. First element: numeric, array of recruits by return year with three dimensions: population (i), return year (y), and
#' age (a). Second element: numeric, array of recruits by brood year with three dimensions: population (i), brood year (b), and
#' age (a). Third element: data frame version of first and second elements, for plotting and tables.
#'#'
#' @format ## `ex_R_star`
#' A list with three elements:
#'
#' `R_star`
#'
#' Array with three dimensions:
#' \describe{
#'        \item{i}{Population}
#'        \item{y}{Return year}
#'        \item{a}{Age}
#'        }
#'
#'  `R_star_b`
#'
#'  Array with three dimensions:
#'  \describe{
#'        \item{i}{Population}
#'        \item{b}{Brood year}
#'        \item{a}{Age}
#'        }
#'
#'  `df`
#'
#'  A dataframe with six variables:
#'  \describe{
#'        \item{i}{Population}
#'        \item{y}{Return year}
#'        \item{a}{Age}
#'        \item{R_star}{Recruits}
#'        \item{b}{Brood year}
#'        \item{complete_brood}{Whether the return year includes all brood years}
#' }
#'
#'
#' @source data-raw/make-example-data.R, get_R_star()
"ex_R_star"
