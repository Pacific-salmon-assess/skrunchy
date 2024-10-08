% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-P-tilde.R
\name{get_P_tilde}
\alias{get_P_tilde}
\title{Get population proportions}
\usage{
get_P_tilde(P, sigma_P, G, save_csv = FALSE)
}
\arguments{
\item{P}{Numeric, array of genetic proportions with 3 dimensions: i (population), w (statistical week),and y (year)}

\item{sigma_P}{Numeric, array of SD of the genetic proportions, with 3 dimensions: i (population), w (week), and y (year).}

\item{G}{Numeric, array of how many Chinook were caught in the gillnet Tyee Test Fishery by week, with 2 dimensions: w (week) and y (year)}

\item{save_csv}{If TRUE, saves the first year of data as csv files for checking.}
}
\value{
List of 3 elements. First is an array of pooled genetic proportions with 2 dimensions: i (population) and y (year).
Second is an array of SD of pooled genetic proportions with 2 dimensions: i (population) and y (year). Third is a data frame
with P_tilde, sigma_P_tilde, and year merged for plotting and reporting.
}
\description{
Do weekly expansions of catch by weekly genetic proportions, and then sum the weekly expansions by population and year
}
\examples{
library(abind)
library(here)
test <- make_P_G()
P <- test$P
sigma_P <- test$sigma_P
G <- test$G

P_tilde <- get_P_tilde(P = P, sigma_P = sigma_P, G= G)

}
