#' Get stage (adult or jack)
#'
#' Uses Skeena River Tyee Test Fishery biodata to assign stage of return (adult or jack) to Chinook, based on
#' CWT age, scale age, POH, and comments.
#'
#' In skrunchy, the main purpose of this function is to filter the complete biodata to estimate
#' weekly catch of adult Chinook, which are used to do the weekly expansions of GSI proportions.
#' Default POH cutoff length is 450 mm. Note that this is what is used at Kitsumkalum.
#'
#' @param scale_age Character, vector of Gilbert-Rich scale ages (including partial ages) from Sclerochronology Lab.
#' @param cwt_age Numeric, vector of ages from Coded Wire Tags. Gilbert-Rich total age (return year minus brood year).
#' @param POH Numeric, post-orbital hypural length, in mm
#' @param comments Character, vector of comments
#' @param jack_ages Character, vector of which scale_age values to include as jacks. Defaults to c("32", "31", "1M")
#' @param jack_cutoff_length_POH Numeric, single value of what POH length to use as a cutoff for jacks. Defaults to 450 mm.
#'
#' @returns Character, "jack" or "adult"
#'
#' @examples
#' scale_age <- c("31", "32", "42", "52", "62", "1M", "2M", "3M", "4M", NA, NA)
#' cwt_age <- c(3, 3, 4, 5, 6, 3, 4, 5, 6, NA, NA)
#' POH <- c(440, 420, 500, 600, 700, 380, 500, 600, 700, NA, NA)
#' comments <- c("Jack", "Jack", NA, NA, NA, NA, NA, NA, NA, "jack, bitten", "Jack")
#' d <- data.frame(scale_age, cwt_age, POH, comments)
#' d$revised_stage <- get_stage( scale_age = scale_age, cwt_age = cwt_age, POH = POH, comments = comments)
#'
#'
#' @export
get_stage <- function(scale_age, cwt_age, POH, comments, jack_ages = c("32", "31", "1M"), jack_cutoff_length_POH = 450) {

  # check lengths equal
  if(!all( length(scale_age) == c(length(cwt_age), length(POH), length(comments)) ))  {
    stop("Length of input variables not equal.") }
  # New variable for revised stage
  stage <- character( length = length(scale_age) )
  # Main function
  for(i in 1:length(scale_age)) {
  stage[i] <- ifelse( isTRUE(cwt_age[i] <4), "jack", # If CWT age is less than 4, it is a jack
                   ifelse(scale_age[i] %in% jack_ages , "jack",# If scale age is a jack age, it is a jack
                          # # If there is no scale age, CWT age, and length is less than jack cutoff, it is a jack
                          # ifelse( all( is.na(scale_age[i]) && is.na(cwt_age[i]), !is.na(POH[i]) && POH[i] <= jack_cutoff_length_POH ), "jack",
                          # If there is a POH length and it is less than the jack cutoff, it is a jack
                          ifelse( all( !is.na(POH[i]), POH[i] <= jack_cutoff_length_POH ), "jack",
                                  # If there is no length, scale age, or CWT age, and it is jack in comments, it is a jack
                                  ifelse( all( is.na(POH[i]) && is.na(scale_age[i]), is.na(cwt_age[i]) && grepl("Jack|jack", comments[i])), "jack",
                                          "adult"))))
    }
  return(stage)
}
