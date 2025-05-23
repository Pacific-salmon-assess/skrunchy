% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-A-phi.R
\name{get_A_phi}
\alias{get_A_phi}
\title{Get preterminal post-fishery abundance}
\usage{
get_A_phi(MatureRun, r)
}
\arguments{
\item{MatureRun}{Numeric, array of mature run size by population (i), year (y), and age (a).}

\item{r}{Numeric, array of maturation rate by year and age. CTC Chinook model output for Kitsumkalum (KLM).}
}
\value{
List with two elements. First element: numeric, array of preterminal post-fishery abundance with
three dimensions: population (i), year (y), and age (a). Second element: data frame version
of first element for plotting and tables.
}
\description{
Calculate the preterminal post-fishery abundance, which is the abundance of the
cohort after non-net fisheries (e.g., troll), after accounting for maturation rate but
before preterminal net (gillnet, seine) fishery harvest of mature fish. Of the fish that survived
non-net ocean fisheries, the number of fish that matured into the mature run.
}
\examples{
  A_phi <- get_A_phi(MatureRun = ex_MatureRun, r = ex_r)


}
