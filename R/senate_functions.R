#' Get Senate quotas by state
#'
#' @param xml A pointer to an XML media feed object.
#'
#' @return A \code{data.frame}.
#' @export
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

#' Get Senate votes by division
#'
#' @param xml A pointer to an XML media feed object.
#'
#' @return A \code{data.frame}.
#' @export
#'
#' @importFrom xml2 xml_find_all xml_attr xml_name xml_text
get_mediafeed_senate_div <- function(xml) {

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
