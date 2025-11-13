#' Get Alaska style statistical week from a date
#'
#' This is used to group catch at Tyee into stat weeks, for weekly gill net catch
#' and as a utility function. See https://mtalab.adfg.alaska.gov/CWT/reports/sbp_calendar.aspx
#'
#'
#' @param date Date, yyyy-mm-dd format date
#'
#' @returns Integer, which stat week the date corresponds to.
#'
#' @examples
#' d <- data.frame( "date" = seq.Date(ISOdate(2021,1,1), ISOdate(2025,12,31)))
#' d$stat_week <- get_stat_week(d$date)
#'
#' @export
get_stat_week <- function(date) {
  # the "%U" format gives: The week number of the year (Sunday as the first day of the week)
  # as a zero-padded decimal number (e.g. "00" to "53").
  stat_week <- ifelse(
    strftime( ISOdate( as.integer( strftime( date, format = "%Y" )), 1, 1), format = "%A" ) == "Sunday", # is the first day of the given year a Sunday?
    as.integer( strftime( date, format = "%U" )),  # If January 1 is a Sunday (in the given year), then don't add 1 to %U format week because it starts at 1 if January 1 is a Sunday
    as.integer( strftime( date, format = "%U" )) + 1 ) # If January 1 is not a Sunday (in the given year), then %U format week starts at 0 and you have to add 1
}
