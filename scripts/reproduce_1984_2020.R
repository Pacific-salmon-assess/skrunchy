# Reproduce Winther et al. 2024 report analysis

library(here)
library(ggplot2)
library(tidyr)
library(dplyr)
library(latex2exp)
library(skrunchy)
options(scipen = 999)
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
           known_population = "Kitsumkalum", aggregate_population = "Skeena"
           #,
           #save_csv = TRUE
           )
View(X$df)
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

#View(E$df)

# Make fig for powerpoint
#png(here("fig/reproduce_report_X_E.png"), width=8, height=4, units="in", res=600)
ggplot(data = E$df, aes(y = E, x = y, group=i)) +
  geom_point() +
  geom_line() +
  facet_wrap(~factor(i, levels = c("Skeena", pops_keep)), ncol=3, scales = "free_y") +
  ylab("Escapement") +
  xlab("Year") +
  coord_cartesian() +
  geom_hline(aes(yintercept=0)) +
  theme_classic() +
  theme(axis.line.x = element_blank())
#dev.off()

# Plot returns to Terrace and escapement
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
# Why does as have more fish than ad? Should be the same. Maybe there are ties for probability?
hist(as$p)
as
# how many age samples for each population*year
n_age_samples <- as %>% group_by(i, y) %>% summarise(n = n())

as$aggregate <- "Skeena"
at <- table(as$i, as$y, as$a, dnn = c("i", "y", "a"))
atskeena <- table(as$aggregate, as$y, as$a, dnn = c("i", "y", "a"))
aarrcu <- as.array(at)
aarrskeena <- as.array(atskeena)
aarr <- abind(aarrcu, aarrskeena, along=1, use.dnns = TRUE)
dimnames(aarr)
aarr <- aarr[ , as.character(1984:2020), ]
dim(aarr)
dimnames(aarr)

aarr

# Get omega age proportions, ages 4-7
omega <- get_omega( aarr[ c("Skeena", pops_keep), , 3:6] , save_csv = TRUE)
omega$omega[,,"7"]

# FLAG: need to check omega values against report values

dimnames(omega$omega)

ggplot( omega$df, aes(y = omega, x = y, group = i)) +
  geom_point() +
  geom_line() +
  geom_hline(aes(yintercept=0)) +
  ylab(TeX("$\\Omega$") )+
  facet_grid( i ~ a ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))

# read in Kitsumkalum age-specific escapement
ksd <- read.csv(here("data-old", "K_star.csv"))
ksdl <- ksd %>% pivot_longer(c("X4", "X5", "X6", "X7"), names_to = "a", values_to = "K_star")
ksdl$a <- sub("X", "", ksdl$a)
dim_order_K_star <- c("y","a")
dim_position_K_star<- sapply(dim_order_K_star, function(x) grep( paste0("^", x, "$"), names(ksdl)))

K_star <- tapply(ksdl$K_star, ksdl[ , dim_position_K_star], FUN=print, default = 0)
K_star
dim(K_star)
dimnames(K_star)
# Age specific escapement
E_star <- get_E_star(E = E$E, omega = omega$omega, use_alternate_escapement_by_age = TRUE,
                     K_star = K_star, save_csv = TRUE, add_6_7 = TRUE)

E_star$df
View(E_star$df)
E_rep <- read.csv(here("data-old", "E-report.csv"))

E_check <- merge(E_star$df, E_rep, by.x = c("y", "a", "b", "i"),
                 by.y = c("RY", "Age", "BY", "CU"), all.x = TRUE)

ggplot(E_check, aes(y = E_star, x = E.report)) +
  geom_point() +
  facet_wrap(~i, scales= "free")

check <- E_check$E.report - E_check$E_star
hist(check)
check

ggplot( E_star$df, aes(y = E_star, x = y, group = i)) +
  geom_point( colour="gray") +
  geom_line( colour="gray") +
  geom_hline(aes(yintercept=0)) +
  facet_grid( i ~ a , scales = "free_y") +
  ylab("Escapement (E*)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))

# Read in brood removals
brood <- read.csv(here("data-old", "brood.csv"))
bdims <- c("y", "a")
bdims_pos <- sapply(bdims, function(x) grep( paste0("^", x, "$"), names(brood)))
B_star <- tapply(brood$B_star, brood[ ,bdims_pos ], FUN = print, default = 0 )

B_star
dim(B_star)
dimnames(B_star)

S_star <- get_S_star(E_star = E_star$E_star, B_star= B_star , save_csv = TRUE)
S_star$df

ggplot( E_star$df, aes(y = E_star, x = y, group = i)) +
  geom_point(colour="gray") +
  geom_line(colour="gray") +
  geom_line(data = S_star$df, aes(y = S_star, x = y, group = i), colour="dodgerblue") +
  geom_hline(aes(yintercept=0)) +
  facet_grid( i ~ a , scales = "free_y") +
  ylab("Escapement (E*) in gray and spawners (S*) in blue") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))

# Get hatchery origin spawners
H_df <- read.csv(here("data-old", "H.csv"))
H_df$H <- round(H_df$H)
H_df <- H_df[!H_df$a == 3, ]
dim_order_H <- c("y","a")
dim_position_H <- sapply(dim_order_H, function(x) grep( paste0("^", x, "$"), names(H_df)))
H_star <- tapply(H_df$H, H_df[ , dim_position_H ], FUN = print, default = 0 )

W_star <- get_W_star(S_star = S_star$S_star, H_star = H_star )

ggplot( E_star$df, aes(y = E_star, x = y, group = i)) +
  geom_point(colour="gray") +
  geom_line(colour="gray") +
  geom_line(data = S_star$df, aes(y = S_star, x = y, group = i), colour="dodgerblue") +
  geom_line(data = W_star$df, aes(y = W_star, x = y, group = i), colour="firebrick") +
  geom_hline(aes(yintercept=0)) +
  facet_grid( i ~ a , scales = "free_y") +
  ylab("Escapement (E*) in gray and spawners (S*) in blue") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))


# Get proportion wild
p <- get_p(W_star = W_star$W_star, E_star = E_star$E_star)
ggplot(p$df, aes(y = p, x = y, group = i)) +
  geom_point() +
  geom_line() +
  facet_grid(i~a) +
  ylab("Proportion wild spawners, p") +
  geom_hline(aes(yintercept=0)) +
  theme_classic()
