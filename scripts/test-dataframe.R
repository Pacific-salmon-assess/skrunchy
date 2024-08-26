rm(list=ls())

# Make some fake data

library(rrandvec)
library(abind)
library(ggplot2)
library(dplyr)

catch_values <- seq(0,100,1) # fake weekly catch values
n_weeks<- 12 # number of stat weeks in each fishing year
weeks <- 1:n_weeks
n_years <- 40 # number of years
years <- seq(1980, length.out=n_years) # years
populations <- c("Kitsumkalum", "Lower Skeena", "Middle Skeena", "Zymoetz-Fiddler", "Large Lakes", "Upper Skeena") # populations
n_populations <- length(populations)

# make df of weekly catches
catch <- expand.grid(w = weeks, y = years)
catch$G <- sample( x = catch_values, size = nrow(catch),  replace=TRUE )



# make data frame of genetic mixture proportions (add to 1 for each week:year)
prop <- expand.grid(w = weeks, y= years, i = populations) %>% arrange( y , w)
P_get <- rrandvec( n = n_weeks*n_years, d = n_populations)
prop$P <- as.numeric(t(P_get))

get_P_tilde <- function(prop, catch) {
  w <- prop$w
  y <- prop$y
  i <- prop$i
  P <- prop$P
  prop$P_times_catch <- mapply( 
    # function to iterate over multiple vectors
    FUN = function( w, i, y, P, catch) {
               P_times_catch <- P * catch$G[ intersect(which(catch$w == w), which(catch$y == y))]
               P_times_catch 
              }, 
          w, i, y, P, # vectors to iterate function over
        MoreArgs = list(catch)) # need catch data frame for weekly catch values
  # Add up weekly catches by population, then divide by yearly catch totals to get
  # estimated yearly proportin, P_tilde
  P_tilde <- prop %>% group_by(y, i) %>% summarise(sum_year_catch = sum(P_times_catch)) %>%
        mutate(P_tilde = sum_year_catch / sum(sum_year_catch))
  P_tilde
}

pool_prop <- get_P_tilde(prop = prop, catch= catch)

test <- pool_prop %>% group_by(y) %>% summarise(sum(P_tilde))

