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
  nms2
}
# Read in data, use character class for all until done checking, fixing errors
path1 <- "1973-2013-tyee-biodata.xlsx"
d1 <- read_xlsx( here("data", "tyee-sampling-biodata-ages", path1) , col_types = "text", .name_repair = fix_names )
d1 <- d1[ d1$species=="124", ] # keep chinook only
path2 <- "2014-2023-tyee-chinook-biodata.xlsx"
d2 <- read_xlsx( here("data", "tyee-sampling-biodata-ages", path2) , , col_types = "text", .name_repair = fix_names)

# Check column names
c(names(d1), names(d2))
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

# get records that were actually aged
aged <- d[ !is.na(as.numeric(d$age)), ]
ggplot(aged, aes(x = as.factor(age))) +
  geom_histogram(stat = "count")
ggplot(aged, aes(x = as.numeric(year_catch))) +
  geom_histogram(stat = "count")

ggplot(ws, aes(x = as.numeric(year_catch))) +
  geom_histogram()
ggplot(d, aes(x = as.numeric(year_catch))) +
  geom_histogram()
