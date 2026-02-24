#' Example recruits by population and brood year for testing and examples
#'
#' List with three elements. First element: numeric, array of recruits by return year with three dimensions: population (i), return year (y), and
#' age (a). Second element: numeric, array of recruits by brood year with three dimensions: population (i), brood year (b), and
#' age (a). Third element: data frame version of first and second elements, for plotting and tables.
#'#'
#' @format ## `ex_R_recruits`
#' A list with three elements:
#'
#' `R`
#'
#' Array with three dimensions:
#' \describe{
#'        \item{i}{Population}
#'        \item{b}{Brood year}
#'        \item{a}{Age}
#'        }
#'
#'  `df`
#'
#'  Data frame with columns:
#'  \describe{
#'        \item{i}{Population}
#'        \item{b}{Brood year}
#'        \item{a}{Age}
#'        \item{complete_brood}{Logical, whether the brood year is complete}
#'        \item{R}{recruits}
#'        }
#'
#'  `df_ages`
#'
#'  A dataframe with six variables:
#'  \describe{
#'        \item{i}{Population}
#'        \item{y}{Return year}
#'        \item{a}{Age}
#'        \item{R_star}{Recruits by age}
#'        \item{b}{Brood year}
#'        \item{complete_brood}{Logical, whether the return year includes all brood years}
#' }
#'
#'
#' @source data-raw/make-example-data.R, get_R()
"ex_R_recruits"
