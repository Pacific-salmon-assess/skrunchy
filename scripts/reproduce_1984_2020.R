# Reproduce Winther et al. 2024 report analysis

library(here)
library(ggplot2)
library(tidyr)

# read in key of genetic baseline collections and Conservation Unit names.
key <- read.csv(here ("data", "population-key.csv"))
# read in data with GSI mixture analysis by week for Tyee
d <- read.csv(here("data-old", "tyee-genetics-weekly-catch.csv"))
# rename columns to match skrunchy package conventions
names(d) <- sub("YEAR", "y", names(d))
names(d) <- sub("WEEK", "w", names(d))
# which columns are GSI proportions?
p_cols <- grep("^P\\.(?!SD\\.).*", names(d), perl=TRUE)
# which columns are GSI proportion SD?
sd_cols <- grep("^P\\.(?=SD\\.).*", names(d), perl=TRUE)
# get just proportions
dp <- d[ ,c(1,2,p_cols) ]
# pivot to long format
dpl <- pivot_longer( dp, names(d)[p_cols], values_to = "P", names_to = "msat_collection" )
# remove P from in front of population names
dpl$msat_collection <- sub("P\\.", "", dpl$msat_collection)
# merge with conservation unit groupings
dpm <- merge(dpl, key[ , names(key) %in% c("msat_collection", "cu_name")], by="msat_collection" )
# change CU name to i, skrunchy package convention
names(dpm)[grep("cu_name", names(dpm))] <- "i"
# Convert dataframe to array with tapply, sum by Conservation Unit i, fill missing values with 0
# what order dims to use
dim_order <- c("i","w","y")
dim_position <- sapply(dim_order, function(x) grep( paste0("^", x, "$"), names(dpm)))
P <- tapply(dpm$P, dpm[ , dim_position], FUN = sum, na.rm=TRUE, default = 0 )
P
dim(P)
dimnames(P)
# Get just SD columns
dsd <- d[ ,c(1,2, sd_cols)]
# convert to long format
dsd <- pivot_longer( dsd, names(d)[sd_cols], values_to = "sigma_P", names_to = "msat_collection" )
# remove P.SD. from in front of CU name
dsd$msat_collection <- sub("P\\.SD\\.", "", dsd$msat_collection)
# merge with CU names
dsdm <- merge(dsd, key[ , names(key) %in% c("msat_collection", "cu_name")], by = "msat_collection" )
# change CU to i, skrunchy convention
names(dsdm)[grep("cu_name", names(dsdm))] <- "i"
# function to add standard deviations
add_sd <- function(x) { sqrt( sum(x^2, na.rm=TRUE) )}
# add SD together by Conservation Unit
# Convert dataframe to array with tapply, sum SD by Conservation Unit, fill missing values with 0
dim_order_SD <- c("i","w","y")
dim_position_SD <- sapply(dim_order_SD, function(x) grep( paste0("^", x, "$"), names(dsdm)))
sigma_P <- tapply(dsdm$sigma_P, dsdm[ , dim_position_SD], FUN = add_sd, default = 0 )
sigma_P
dim(sigma_P)
dimnames(sigma_P)
# pull out Tyee test fishery gillnet revised weekly catch associated with each GSI mixture analysis
dg <- d[ , c(1,2,4)]
# rename
names(dg)[3] <- "G"
# Convert dataframe to array with tapply, fill missing values with 0
dim_order_g <- c("w","y")
dim_position_g <- sapply(dim_order_g, function(x) grep( paste0("^", x, "$"), names(dg)))
G <- tapply(dg$G, dg[ , dim_position_g], FUN = print, default = 0 )
G
dim(G)
dimnames(G)
# check dimensions equal
dim(P)
dim(sigma_P)
dim(G)
# Expand genetic proportions by week and add together to get annual proportions
#   corrected for sample size and changing run timing over the year.
P_tilde <- get_P_tilde( P = P, sigma_P = sigma_P, G = G)

P_tilde$df_merged
View(P_tilde$df_merged)

# Preliminary look at values, they are exactly the same as in "1AB Skeena Esc 1979 to 2020 POPAN 2023-11-13 to LW&CM .xlsx"
# tab "CU esc calc POPAN". That is fantastic! Only found one minor difference in Large Lakes percent for 2019.


ggplot(P_tilde$df_merged, aes(x = y , y = P_tilde)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes( ymin = P_tilde - sigma_P_tilde, ymax= P_tilde + sigma_P_tilde)) +
  facet_wrap(~i) +
  #geom_hline(aes(yintercept=0)) +
  coord_cartesian(expand=FALSE, clip="off") +
  theme_bw()

