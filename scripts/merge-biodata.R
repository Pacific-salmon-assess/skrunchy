# Merge old and new Skeena Tyee Test Fishery biodata / sampling data / age.

# These are from an older access database and a newer (current) access database.


# See scripts written by Michael Folkes for error fixing:
# https://gitlab.com/MichaelFolkes/biodata/-/blob/main/scripts/biosample_dbase_newkeys_v2.R?ref_type=heads
# https://gitlab.com/MichaelFolkes/biodata/-/blob/main/scripts/biosample_data_2014-2020_errors.R?ref_type=heads



library(here)
library(readxl)
library(ggplot2)


fix_names <- function(nms) {
  nms0 <- gsub("VESSEL\\(CFV\\)", "VESSEL_CFV", nms)
  nms1 <- tolower(gsub("\\s", "_", nms0))
  nms2 <- gsub("\\)|\\(", "", nms1)
  nms3 <- gsub("color", "colour", nms2)
  nms4 <- gsub("dna_vial", "vial", nms3)
  nms4
}
# Read in data, use character class for all until done checking, fixing errors
path1 <- "1973-2013-tyee-biodata.xlsx"
path1a <- "1973-2013-tyee-biodata-all-cols.xlsx"
#d1 <- read_xlsx( here("data", "tyee-sampling-biodata-ages", path1) , col_types = "text", .name_repair = fix_names )
d1 <- read_xlsx( here("data", "tyee-sampling-biodata-ages", path1a) , col_types = "text", .name_repair = fix_names )
d1 <- d1[ d1$species=="124", ] # keep chinook only
# remove unneccessary columns
cols_not_use <- c("wab_code", "stream_name", "vtp", "agency", "freshwater_sample_site_location",
                  "longitude", "latitude", "rab_code", "site_code", "sub-area",
 "reach_number", "fish_wheel_number", "first_year_freshwater_circulii_count",
 "second_year_freshwater_circilii_count", "third_year_freshwater_circulii_count",
 "lifestage", "fish_number", "maturity",
 "fin_ray_box_number", "fin_ray_number",# only in 1996
 #"otolith_box_number", "otolith_number",
 "body_weight_gm",   # 1973 - 1994 sporadically
 "rd/drs", "grade", "gonad_sample_number", "gonad_weight_gm",
 "fecundity_estimate_type", "fecundity_estimate", # 1975-1977 only
 "egg_retention", "carcass_condition", "skin_colour", "external_tag_colour",
 "external_tag", "external_tag_2nd_tag", "radiotag_frequency",
 #"coded_fin_mark", "head_code", "head_number", "cwt_code", # these might be superfluous for this data
 "external_damage", "hook_location",
 "stomach_sample", "stomach_volume_cc", "type_of_parasite_sample",
 "parasite_sample_number", "presence_of_philonema_species_worms", "disease_type",
 "disease_preservation",  "trap_fishing", "water_level",
 "comments",  "field174",  "field176",  "field178" )
d1 <- d1[ , !names(d1) %in% cols_not_use]

# Sampling event day different than catch day for 2 records:
dif_sample_catch_day <- d1[ as.integer(d1$day_catch) - as.integer(d1$day_sampling_event)>0, ]
# weird, catch day after sampling event day???
d1 <- d1[!is.na(d1$species), ] # remove NA rows
# fix typo
d1$location_name <- sub("SLEENA", "SKEENA", d1$location_name) # one typo in 1996

path2 <- "2014-2023-tyee-chinook-biodata.xlsx"
d2 <- read_xlsx( here("data", "tyee-sampling-biodata-ages", path2) , , col_types = "text", .name_repair = fix_names)
cols_not_use2 <- c("months_catch", "field1")
d2 <- d2[ , !names(d2) %in% cols_not_use2]


# Check column names
n1 <- names(d1)
n2 <- names(d2)
n1
n2
length(n1)
length(n2)

union(n1, n2)
intersect(n1, n2)

setdiff(n1, n2)
setequal(n1, n2)

is.element(n1, n2)
is.element(n2, n1)

nd1 <- data.frame(names = n1, data = "old")
nd2 <- data.frame(names = n2, data = "new")
dm <- merge(nd1, nd2, by="names", all=TRUE, suffixes = c("old", "new"))
#write.csv( dm, here("data-out", "biodata-names-compare.csv"), row.names = FALSE)

# bind data frames
str(d1)
str(d2)

d <- dplyr::bind_rows(d1, d2)
write.csv( d, here("data-out", "biodata-merge-no-fix.csv"), row.names = FALSE)



as.numeric("1M")


# get records that were actually aged
aged <- d[ !is.na(as.numeric(d$age)), ]

# look at sex
unique(d$sex)

# 2019 was entered in EU age????
euage <- aged[ grep("^0.*|^1.*", aged$age), ]
unique(euage$year_catch)

table(d[d$year_catch=="2019", "age"])

# Fin ray number
# only in 1996


# gillnet type

# Seine in 1988?



# plot

ggplot(aged, aes(x = as.factor(age))) +
  geom_histogram(stat = "count")
ggplot(aged, aes(x = as.numeric(year_catch), fill= sex)) +
  geom_histogram(stat = "count")
ggplot(aged, aes(x = as.numeric(year_catch), fill= age)) +
  geom_histogram(stat = "count")

ggplot(ws, aes(x = as.numeric(year_catch))) +
  geom_histogram()

ggplot(d, aes(x = as.numeric(year_catch))) +
  geom_histogram()
