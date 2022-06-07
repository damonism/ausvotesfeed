#' Get Senate quotas by state
#'
#' Get AEC calculated Senate quotes and vacancies by jurisdiction from the media
#' feed file.
#'
#' This returns the current quota and number of vacancies from the media feed
#' file for the Senate elections.
#'
#' Note that if you are planning on using \code{\link{get_mediafeed_votes_sen}}
#' that provides the votes as quotas through the \code{QuotaProportion}
#' variable.
#'
#' This can also be used on the preload XML file, although it will return all of
#' the \code{Quota} variables as 1. This is mostly useful for functions that
#' need the \code{NumberOfPositions} variables.
#'
#' @param xml A pointer to an XML media feed object.
#'
#' @return A \code{data.frame} with four variables: \code{StateAb},
#'   \code{Quota}, \code{QuotaProvisional} (\code{logical}) and
#'   \code{NumberOfPositions}.
#' @export
#'
#' @examples
#' results_xml <- read_mediafeed_xml(get_mediafeed_file(2022, "Verbose", Archive = TRUE))
#' get_mediafeed_senate_quotas(results_xml)
#'
#' @importFrom xml2 xml_find_all xml_attr xml_text
get_mediafeed_senate_quotas <- function(xml) {

  tmp_df <- data.frame(StateAb = xml_attr(xml_find_all(xml, "/d1:MediaFeed/d1:Results/d1:Election/d1:Senate/d1:Contests/d1:Contest/eml:ContestIdentifier"), "Id"),
                       Quota = xml_text(xml_find_all(xml, "/d1:MediaFeed/d1:Results/d1:Election/d1:Senate/d1:Contests/d1:Contest/d1:Quota")),
                       QuotaProvisional = xml_attr(xml_find_all(xml, "/d1:MediaFeed/d1:Results/d1:Election/d1:Senate/d1:Contests/d1:Contest/d1:Quota"), "Provisional"),
                       NumberOfPositions = xml_text(xml_find_all(xml, "/d1:MediaFeed/d1:Results/d1:Election/d1:Senate/d1:Contests/d1:Contest/d1:NumberOfPositions"))
  )
  tmp_df$Quota <- as.integer(tmp_df$Quota)
  tmp_df$NumberOfPositions = as.integer(tmp_df$NumberOfPositions)
  tmp_df$QuotaProvisional <- ifelse(tmp_df$QuotaProvisional == "true", TRUE, FALSE)

  return(tmp_df)

}

#' Get Senate votes by state
#'
#' Get the Senate first preference votes from the media feed XML file by state
#' and territory.
#'
#' Similar to the equivalent House functions, this function attempts to return a
#' relatively minimalist table of results that is designed to be merged with the
#' output of \code{\link{get_mediafeed_preload_candidates_sen}}.
#'
#' There are three possible values for \code{CandidateType}:
#'
#' \itemize{
#'
#' \item{\code{Candidate}}{A candidate in a group}
#'
#' \item{\code{TicketVotes}}{The total number of votes above the line for that
#' group}
#'
#' \item{\code{Unapportioned}}{According to the
#' \href{https://www.aec.gov.au/footer/glossary.htm#u}{AEC's definitions},
#' unapportioned votes are "are votes that have been allocated to a party or
#' group by one count but have not yet been allocated to individual candidates
#' within that party or group. As the counting progresses, these votes will be
#' distributed to the individual candidates."}
#'
#' \item{\code{UngroupedCandidate}}{A candidate not in a Senate group (that is,
#' without a box above the line).}
#'
#' }
#'
#' The media feed also provides a \code{GroupTotal} result, but for the sake of
#' convenience that is not included in the output of this function because it
#' would require filtering out before aggregating votes.
#'
#' @param xml A pointer to an XML media feed object.
#'
#' @return A \code{data.frame} with 13 variables: \code{StateAb},
#'   \code{CandidateType} (can be one of either \code{Candidate},
#'   \code{TicketVotes}, \code{Unapportioned} or \code{UngroupedCandidate}),
#'   \code{Ticket}, \code{GroupId}, \code{CandidateId}, \code{PartyId},
#'   \code{IsIndependent}, \code{Historic} (votes), \code{Percentage},
#'   \code{Swing}, \code{QuotaProportion} and \code{Votes}.
#' @export
#'
#' @examples
#' results_xml <- read_mediafeed_xml(get_mediafeed_file(2022, "Verbose", Archive = TRUE))
#' get_mediafeed_senate_quotas(results_xml)
#'
#' @importFrom xml2 xml_find_all xml_attr xml_name xml_text
get_mediafeed_votes_sen <- function(xml) {

  tmp_nodes <- xml_find_all(xml, paste("/d1:MediaFeed/d1:Results/d1:Election/d1:Senate/d1:Contests/d1:Contest/eml:ContestIdentifier",
                                       "/d1:MediaFeed/d1:Results/d1:Election/d1:Senate/d1:Contests/d1:Contest/d1:FirstPreferences/d1:Group/d1:GroupIdentifier",
                                       "/d1:MediaFeed/d1:Results/d1:Election/d1:Senate/d1:Contests/d1:Contest/d1:FirstPreferences/d1:Group/d1:Candidate",
                                       "/d1:MediaFeed/d1:Results/d1:Election/d1:Senate/d1:Contests/d1:Contest/d1:FirstPreferences/d1:Group/d1:TicketVotes",
                                       "/d1:MediaFeed/d1:Results/d1:Election/d1:Senate/d1:Contests/d1:Contest/d1:FirstPreferences/d1:Group/d1:Unapportioned",
                                       "/d1:MediaFeed/d1:Results/d1:Election/d1:Senate/d1:Contests/d1:Contest/d1:FirstPreferences/d1:UngroupedCandidate",
                                       sep = "|"))

  tmp_df <- data.frame(CandidateType = xml_name(tmp_nodes),
                       id = xml_attr(tmp_nodes, "Id"),
                       Ticket = xml_text(xml_find_first(tmp_nodes, "d1:Ticket")),
                       CandidateId = xml_attr(xml_find_first(tmp_nodes, "eml:CandidateIdentifier"), "Id"),
                       PartyId = xml_attr(xml_find_first(tmp_nodes, "eml:AffiliationIdentifier"), "Id"),
                       PartyAb = xml_attr(xml_find_first(tmp_nodes, "eml:AffiliationIdentifier"), "ShortCode"),
                       IsIndependent = xml_attr(tmp_nodes, "Independent"),
                       Historic = xml_attr(xml_find_first(tmp_nodes, "d1:Votes"), "Historic"),
                       Percentage = xml_attr(xml_find_first(tmp_nodes, "d1:Votes"), "Percentage"),
                       Swing = xml_attr(xml_find_first(tmp_nodes, "d1:Votes"), "Swing"),
                       QuotaProportion = xml_attr(xml_find_first(tmp_nodes, "d1:Votes"), "QuotaProportion"),
                       Votes = xml_text(xml_find_first(tmp_nodes, "d1:Votes")),
                       stringsAsFactors = FALSE)

  tmp_df$StateAb <- ifelse(tmp_df$CandidateType == "ContestIdentifier", tmp_df$id, NA)
  tmp_df$StateAb <- Fill(tmp_df$StateAb)
  tmp_df$GroupId <- ifelse(tmp_df$CandidateType == "GroupIdentifier", tmp_df$id, NA)
  tmp_df$GroupId[!tmp_df$CandidateType %in% c("ContestIdentifier", "UngroupedCandidate")] <- Fill(tmp_df$GroupId[!tmp_df$CandidateType %in% c("ContestIdentifier", "UngroupedCandidate")])
  tmp_df$Ticket[!tmp_df$CandidateType %in% c("ContestIdentifier", "UngroupedCandidate")] <- Fill(tmp_df$Ticket[!tmp_df$CandidateType %in% c("ContestIdentifier", "UngroupedCandidate")])
  tmp_df <- tmp_df[!tmp_df$CandidateType %in% c("ContestIdentifier", "GroupIdentifier"),]
  tmp_df <- tmp_df[c("StateAb", "CandidateType", "Ticket", "GroupId", "CandidateId", "PartyId", "PartyAb", "IsIndependent", "Historic", "Percentage", "Swing", "QuotaProportion", "Votes")]

  tmp_df[c("GroupId", "CandidateId", "PartyId", "Historic", "Votes")] <- sapply(tmp_df[c("GroupId", "CandidateId", "PartyId", "Historic", "Votes")], as.integer)
  tmp_df[c("Percentage", "Swing", "QuotaProportion")] <- sapply(tmp_df[c("Percentage", "Swing", "QuotaProportion")], as.numeric)
  tmp_df$IsIndependent <- ifelse(is.na(tmp_df$IsIndependent), FALSE,
                                 ifelse(tmp_df$IsIndependent == "true", TRUE, FALSE))

  return(tmp_df)
}
