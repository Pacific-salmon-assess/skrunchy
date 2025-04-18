% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-W.R
\name{get_W}
\alias{get_W}
\title{Get wild spawners (natural origin) by population}
\usage{
get_W(
  method = "sum_ages",
  W_star,
  S,
  H,
  hatchery_population = "Kitsumkalum",
  aggregate_population = "Skeena"
)
}
\arguments{
\item{method}{How to calcute spawners (W)? If "sum_ages", take the sum across ages of W_star (age-specific wild spawner array).
If "subtract_hatchery", uses non-age-specific spawners (S) and subtracts non-age-specific hatchery spawners (H). Defaults to sum_ages.}

\item{W_star}{Numeric, array of wild spawner values with three dimensions:  population (i), year (y), and age (a).}

\item{S}{numeric, matrix of spawner values for Chinook with two dimensions: population (i), and year (y). Chinook that spawned (doesn't include brood stock removals).}

\item{H}{Numeric, vector of number of hatchery origin spawners returning to the population with a hatchery (e.g., Kitsumkalum River), by year (y).
This is the sum of CTC model output column Esc_exp by return year, for KLM (fry) and KLY (yearlings) combined.}

\item{hatchery_population}{Character, name of population i with where brood removals occurred. Defaults to Kitsumkalum.}

\item{aggregate_population}{Character, name of population i that is the sum of the other populations. Defaults to Skeena. Brood also need to removed from this since brood population is part of the aggregate.}
}
\value{
List with two elements. First element: numeric, array of wild spawner values for Chinook with two dimensions: population (i), and year (y).
Second element: data frame version of matrix, for plotting and tables.
}
\description{
Calculates wild spawners (W, not age specific) by population by either summing age-specific wild spawners (W_star) across ages, or subtracting hatchery-origin spawners (H) from spawners (S) for Kitsumkalum and Skeena aggregate populations.
All other populations, wild spawners (W) = spawners (S).
Not age-specific.
}
\examples{
# Sum ages method
  W <- get_W(method = "sum_ages", W_star = ex_W_star)

# Subtract hatchery method
 W <- get_W( method = "subtract_hatchery", S = ex_S, H = ex_H)

}
