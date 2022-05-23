#' VTR Ids
#'
#' A \code{data.frame} with EventIds and ElectionYear for use in other functions.
#'
#' This is designed to be the one place with a mapping between election years
#' and AEC election identifiers.
#'
#' @return A \code{data.frame} with one column of \code{EventId} and
#'         one column \code{ElectionYear}.
#'
vtr_ids <- function() {
  data.frame(EventId = c(12246, 13745, 15508, 17496, 20499, 24310, 27966),
             ElectionYear = c(2004, 2007, 2010, 2013, 2016, 2019, 2022),
             stringsAsFactors = FALSE)
}

#' VTR Identifier as Year or EventId
#'
#' Convert an event identifier or an election year into either an event identifier
#' or an election year.
#'
#' The Australian Electoral Commission (AEC) identifies each Australian federal
#' election and by-election with a (so far) five digit event identifier. This is
#' used in the labelling of the media feed files and in the file paths of the
#' files on the virtual tally room, but is not easy to remember (or determine).
#'
#' This function allows other functions that require an election identifier to
#' accept either a year or an election identifier. For convenience it also
#' works the other way around, in case you need that.
#'
#' @param identifier One of either an election year or an AEC event identifier
#' @param as One of either \code{year} or \code{event}
#'
#' @return Either the election year or event identifier as a numeric.
#' @export
#'
#' @examples
#' vtr_identifier(27966, "year")
#'
#' vtr_identifier(2022, "event")
vtr_identifier <- function(identifier, as) {

  event_id_df <- vtr_ids()

  if(toupper(as) == "YEAR") {

    if(identifier %in% event_id_df$ElectionYear) {
      return(identifier)
    } else if (identifier %in% event_id_df$EventId) {
      return(event_id_df$ElectionYear[event_id_df$EventId == identifier])
    } else {
      stop(identifier, " is not a valid identifier or election year.")
    }

  } else if(toupper(as) == "EVENT") {

    if(identifier %in% event_id_df$EventId) {
      return(identifier)
    } else if (identifier %in% event_id_df$ElectionYear) {
      return(event_id_df$EventId[event_id_df$ElectionYear == identifier])
    } else {
      stop(identifier, " is not a valid identifier or election year.")
    }

  } else {

    stop("'as' should be one of 'year' or 'event'.")

  }

}
