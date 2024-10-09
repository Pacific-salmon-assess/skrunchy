# Reproduce Winther et al. 2024 report analysis

library(here)
library(ggplot2)
library(tidyr)

# Note: Winther et al. 2024  report included Kuldo Creek in Middle Skeena (which makes sense geographically),
# But on the dendodgrams for MSAT and SNP it is more closely related to Middle Skeena
# and is included in Middle Skeena groupings for both MSAT and SNP.
# Need to address for future work.

pops_keep <- c("Kitsumkalum", "Large Lakes", "Lower Skeena", "Middle Skeena",
               "Upper Skeena", "Zymoetz-Fiddler")
# read in key of genetic baseline collections and Conservation Unit names.
key <- read.csv(here ("data", "population-key.csv"))
# read in data with GSI mixture analysis by week for Tyee
d <- read.csv(here("data-old", "tyee-genetics-weekly-catch.csv"))
# rename columns to match skrunchy package conventions
names(d) <- sub("YEAR", "y", names(d))
names(d) <- sub("WEEK", "w", names(d))
d <- d[d$y>=1984, ] # remove years before 1984, we don't have Kitsumkalum estimates before that.
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
#dpm <- merge(dpl, key[ , names(key) %in% c("msat_collection", "cu_name")], by="msat_collection" )
dpm <- merge(dpl, key[ , names(key) %in% c("msat_collection", "cu_name_match_report")], by="msat_collection" )

# change CU name to i, skrunchy package convention
#names(dpm)[grep("cu_name", names(dpm))] <- "i"
names(dpm)[grep("cu_name_match_report", names(dpm))] <- "i"
# only keep CUs to use for run reconstruction
dpm <- dpm[  dpm$i  %in% pops_keep, ]

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
#dsdm <- merge(dsd, key[ , names(key) %in% c("msat_collection", "cu_name")], by = "msat_collection" )
dsdm <- merge(dsd, key[ , names(key) %in% c("msat_collection", "cu_name_match_report")], by = "msat_collection" )

# change CU to i, skrunchy convention
#names(dsdm)[grep("cu_name", names(dsdm))] <- "i"
names(dsdm)[grep("cu_name_match_report", names(dsdm))] <- "i"
# only keep CUs to use for run reconstruction
dsdm <- dsdm[  dsdm$i  %in% pops_keep, ]

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
P_tilde <- get_P_tilde( P = P, sigma_P = sigma_P, G = G, save_csv = TRUE)

P_tilde$df

# Preliminary look at values, they are extremely close to "1AB Skeena Esc 1979 to 2020 POPAN 2023-11-13 to LW&CM .xlsx"
# tab "CU esc calc POPAN". That is fantastic! Most differences small enough to be rounding errors since
# excel column values look like they were copy pasted from formula cells in another tab.

# Difference in SD for 2009 for Kitsumkalum.

ggplot(P_tilde$df, aes(x = y , y = P_tilde)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes( ymin = P_tilde - sigma_P_tilde, ymax= P_tilde + sigma_P_tilde)) +
  facet_wrap(~i) +
  #geom_hline(aes(yintercept=0)) +
  coord_cartesian(expand=FALSE, clip="off") +
  theme_bw()

# Read in Kitsumkalum data
kd <- read.csv(here("data-old", "kitsumkalum.csv"))
head(kd)

X <- get_X(P_tilde = P_tilde$P_tilde, sigma_P_tilde = P_tilde$sigma_P_tilde, K = kd$spawners, sigma_K = kd$SE, y = kd$year,
           known_population = "Kitsumkalum", aggregate_population = "Skeena",
           save_csv = TRUE)

ggplot(X$df, aes(y = X, x = y, group = i)) +
  geom_errorbar( aes( ymin = X - sigma_X, ymax = X + sigma_X)) +
  geom_point() +
  geom_line() +
  facet_wrap(~i, ncol=2, scales = "free_y") +
  geom_hline(aes(yintercept=0)) +
  theme_classic()

# Differences in 2009, 2016, 2019. Otherwise perfect for Skeena aggregate.


# Read in total terminal mortality upstream of Terrace
Tau_U_d <- read.csv(here("data-old", "Tau_U.csv"))
Tau_U_d <- Tau_U_d[Tau_U_d$y >=1984, ]


E <- get_E( K = kd$spawners, X = X$X, Tau_U = Tau_U_d$Tau_U)

View(E$df)
ggplot(X$df, aes(y = X, x = y, group = i)) +
  geom_errorbar( aes( ymin = X - sigma_X, ymax = X + sigma_X)) +
  geom_point() +
  geom_line() +
  geom_line(data = E$df, aes(y = E, x = y, group=i), colour="gray") +
  facet_wrap(~i, ncol=2, scales = "free_y") +
  ylab("Return to Terrace (X) in black, escapement (E) in gray") +
  geom_hline(aes(yintercept=0)) +
  theme_classic()

# get age proportions from age data
ad <- read.csv(here("data-old", "ages-with-genetics-individual.csv"))
table(ad$y, ad$a)

names(ad)
names(ad)[names(ad) %in%  key$msat_collection ]
key$msat_collection
al <- pivot_longer(ad, cols = names(ad)[names(ad) %in%  key$msat_collection ],
                   values_to = "p", names_to = "msat_collection")
am <- merge( al, key[ , names(key) %in% c("msat_collection", "cu_name_match_report")], by = "msat_collection" )

names(am)[grep("cu_name_match_report", names(am))] <- "i"
as <- am %>% group_by(JD, Fish, y, i, SEX, a, NOSE.FORK.LENGTH..mm.) %>%
  summarise(p = sum(p, na.rm=TRUE)) %>%
  group_by(JD, Fish, y, SEX, a, NOSE.FORK.LENGTH..mm.) %>%
  slice_max(p)
as
at <- table(as$i, as$y, as$a)
aarr <- as.array(at)
dim(aarr)
dimnames(aarr)
