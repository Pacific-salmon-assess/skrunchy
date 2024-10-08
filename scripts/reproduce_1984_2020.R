# Reproduce 2024 report analysis

library(here)
library(dplyr)
library(tidyr)

key <- read.csv(here ("data", "population-key.csv"))

d <- read.csv(here("data-old", "tyee-genetics-weekly-catch.csv"))
names(d) <- sub("YEAR", "y", names(d))
names(d) <- sub("WEEK", "w", names(d))

p_cols <- grep("^P\\.(?!SD\\.).*", names(d), perl=TRUE)
p_sd_cols <- grep("^P\\.(?=SD\\.).*", names(d), perl=TRUE)

names(d)

names(d)[p_cols]
names(d)[p_sd_cols]

P_df <- d[ ,c(1,2,p_cols) ]
P_df1 <- pivot_longer( P_df, names(d)[p_cols], values_to = "P", names_to = "i" )
P_df1$i <- sub("P\\.", "", P_df1$i)


P_df1m <- merge(P_df1, key[ , names(key) %in% c("msat_collection", "cu_name")], by.x= "i", by.y="msat_collection" )

P_df1s <- P_df1m %>% group_by(cu_name,w,y) %>% summarise( P = sum(P, na.rm=TRUE)) %>% ungroup()
names(P_df1s)[grep("cu_name", names(P_df1s))] <- "i"

# get complete observations, to construct array
P_df2 <- complete(P_df1s, y, w, i, fill = list(P = 0))



P_df2[ order(P_df2$y, P_df2$w, P_df2$i), ]$P
# order P by year, week, and population, then construct array, Note that the ordering variables in data
#   argument need to be the opposite order of the dims and dimnames, because the data fills in the
#   array according to i, w, then y.
P <-  array(data =   P_df2[ order(P_df2$y, P_df2$w, P_df2$i), ]$P ,
            dim=c(length(unique(P_df2$i)),
                  length(unique(P_df2$w)),
                  length(unique(P_df2$y))),
            dimnames=list(i = unique(P_df2$i), w = unique(P_df2$w), y = unique(P_df2$y))
)
P
dim(P)
dimnames(P)

P_SD_df <- d[ ,c(1,2,p_sd_cols)]
sigma_P_df1 <- pivot_longer( P_SD_df, names(d)[p_sd_cols], values_to = "sigma_P", names_to = "i" )
sigma_P_df1$i <- sub("P\\.SD\\.", "", sigma_P_df1$i)

sigma_P_df1m <- merge(sigma_P_df1, key[ , names(key) %in% c("msat_collection", "cu_name")], by.x= "i", by.y="msat_collection" )


add_sd <- function(x) { sqrt( sum(x^2, na.rm=TRUE) )}
sigma_P_df1s <- sigma_P_df1m %>% group_by(cu_name,w,y) %>% summarise( sigma_P = add_sd(sigma_P)) %>% ungroup()
names(sigma_P_df1s)[grep("cu_name", names(sigma_P_df1s))] <- "i"

# get complete observations, to construct array
sigma_P_df2 <- complete(sigma_P_df1s, y, w, i, fill = list(sigma_P = 0))



sigma_P_df2[ order(sigma_P_df2$y, sigma_P_df2$w, sigma_P_df2$i), ]$sigma_P
# order P by year, week, and population, then construct array, Note that the ordering variables in data
#   argument need to be the opposite order of the dims and dimnames, because the data fills in the
#   array according to i, w, then y.
sigma_P <-  array(data =   sigma_P_df2[ order(sigma_P_df2$y, sigma_P_df2$w, sigma_P_df2$i), ]$sigma_P ,
            dim=c(length(unique(sigma_P_df2$i)),
                  length(unique(sigma_P_df2$w)),
                  length(unique(sigma_P_df2$y))),
            dimnames=list( i = unique( sigma_P_df2$i), w = unique(sigma_P_df2$w), y = unique(sigma_P_df2$y))
)
sigma_P
dim(sigma_P)
dimnames(sigma_P)

G_df <- d[ , c(1,2,4)]
names(G_df)[3] <- "G"

G_df1 <- complete(G_df, y, w, fill = list( G= 0))

G <- array( G_df1[ order(G_df1$y, G_df1$w), ]$G,
            dim = c(length(unique(G_df1$w)), length(unique(G_df1$y))),
            dimnames = list( w =  unique(G_df1$w), y = unique(G_df1$y)))
G
dim(G)
dimnames(G)

dim(P)
dim(sigma_P)
dim(G)

P_tilde <- get_P_tilde( P = P, sigma_P = sigma_P, G = G)
P_tilde$df_merged
View(P_tilde$df_merged)

# Preliminary look at values, they are exactly the same as in "1AB Skeena Esc 1979 to 2020 POPAN 2023-11-13 to LW&CM .xlsx"
# tab "CU esc calc POPAN". That is fantastic!

library(ggplot2)
names(P_tilde$df_merged)[2] <- "year"

ggplot(P_tilde$df_merged, aes(x = y , y = P_tilde)) +
  geom_point() +
  geom_line() +
  facet_wrap(~i) +
  theme_classic()

