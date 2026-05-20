library(here)


variable_name_key <- read.csv(here("data-raw/variable-name-key.csv"))

variable_name_key$skrunchy_with_detail <- paste( variable_name_key$skrunchy, variable_name_key$detail, sep = "_")


usethis::use_data(variable_name_key, overwrite = TRUE)

