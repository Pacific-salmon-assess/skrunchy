% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-tau-M.R
\name{get_tau_M}
\alias{get_tau_M}
\title{Get total terminal mortality in marine areas}
\usage{
get_tau_M(W_star, tau_dot_M)
}
\arguments{
\item{W_star}{Numeric, array of wild spawner values for Chinook with two dimensions: population (i), year (y), and age (a).}

\item{tau_dot_M}{Numeric, array of terminal marine exploitation rates with dimensions year (y) and age (a). Sum of exploitation rate for terminal
marine net fisheries and exploitation rate for terminal marine sport fisheries
(coded "TNBC TERM N" and "TNBC TERM S" by the Chinook Technical Committee, respectively), which are outputs from the Kitsumkalum CWT analysis.}
}
\value{
List with two elements. First element: numeric, array of terminal total mortalities in marine fisheries, with three dimensions: population (i), year (y), and age (a).
Second element: data frame version of first element for plotting and tables.
}
\description{
Estimate the total terminal mortality (catch plus incidental mortality) in marine areas.
These include recreational and commercial catch near the Skeena River mouth, including Area 4.
This is based on Coded Wire Tag data and Chinook Technical Committee model output.
}
\examples{
 tau_M <- get_tau_M( W_star = ex_W_star, tau_dot_M = ex_tau_dot_M )


}
