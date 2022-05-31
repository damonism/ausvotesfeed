#' Get votes by polling place
#'
#' Generic function for downloading various vote types (first preference,
#' two-candidate preferred or two-party preferred) by polling place.
#'
#' Note that this iteration of this function does not have any real error
#' checking, and may stop working if it receives unexpected input. The data
#' extraction process itself should be relatively robust, but the type
#' conversions in the function are hardcoded and may fail.
#'
#' @param xml A pointer to an XML media feed object
#' @param type One of either \code{fp} (first preference), \code{tcp}
#'   (two-candidate preferred) or \code{tpp} (two party preferred).
#'
#' @return A \code{data.frame} with a \code{DivisionId} column and at least
#'   current, historic, percentage, swing and matched historic votes.
#' @export
#'
#' @examples
#' \dontrun{
#' results_xml <- read_mediafeed_xml(get_mediafeed_file(2022, "Verbose", Archive = TRUE))
#' get_mediafeed_votes_div(results_xml, "fp")}
#'
#' @importFrom xml2 xml_find_all xml_attr xml_attrs xml_name
get_mediafeed_votes_div <- function(xml, type) {
  # Note: This is the quick and dirty version with no error checking!
  #
  # This could have elected added pretty easily with:
  # xml_find_all(tmp_cand_nodes, "d1:Elected")
  #

  tmp_div_nodes <- xml_find_all(xml, "d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest")

  type <- tolower(type)
  if(type == "fp") {
    tmp_cand_nodes <- xml_find_all(tmp_div_nodes, "d1:PollingDistrictIdentifier|d1:FirstPreferences/d1:Candidate|d1:FirstPreferences/d1:Ghost")
    tmp_vote_prefix <- "FP"
    tmp_list_ints_type <- c("CandidateId", "FP.Historic", "FP.MatchedHistoric", "FP.Votes")
    tmp_list_num_type <- c("FP.Percentage", "FP.Swing")
  } else if (type == "tcp") {
    tmp_cand_nodes <- xml_find_all(tmp_div_nodes, "d1:PollingDistrictIdentifier|d1:TwoCandidatePreferred/d1:Candidate")
    tmp_vote_prefix <- "TCP"
    tmp_list_ints_type <- c("CandidateId", "TCP.Historic", "TCP.MatchedHistoric", "TCP.Votes", "TCP.MatchedHistoricFirstPrefsIn")
    tmp_list_num_type <- c("TCP.Percentage", "TCP.Swing")
  } else if(type == "tpp") {
    tmp_cand_nodes <- xml_find_all(tmp_div_nodes, "d1:PollingDistrictIdentifier|d1:TwoPartyPreferred/d1:Coalition")
    tmp_vote_prefix = "TPP"
    tmp_list_ints_type <- c("CoalitionId", "TPP.Historic", "TPP.MatchedHistoric", "TPP.Votes")
    tmp_list_num_type <- c("TPP.Percentage", "TPP.Swing")
  } else {
    stop("type must be one of 'fp', 'tcp' or 'tpp'.")
  }

  tmp_df <- data.frame(DivisionId = xml_attr(tmp_cand_nodes, "Id"),
                       CandidateType = xml_name(tmp_cand_nodes),
                       stringsAsFactors = FALSE)
  tmp_df$DivisionId <- Fill(tmp_df$DivisionId)
  tmp_df <- tmp_df[tmp_df$CandidateType != "PollingDistrictIdentifier",]
  if(type == "tpp") {
    tmp_df <- data.frame(tmp_df,
                         CoalitionId = xml_attr(xml_find_all(tmp_cand_nodes, "d1:CoalitionIdentifier"), "Id"),
                         CoalitionCode = xml_attr(xml_find_all(tmp_cand_nodes, "d1:CoalitionIdentifier"), "ShortCode"),
                         stringsAsFactors = FALSE)
  } else {
    tmp_df <- data.frame(tmp_df,
                         CandidateId = xml_attr(xml_find_all(tmp_cand_nodes, "eml:CandidateIdentifier"), "Id"),
                         stringsAsFactors = FALSE)

  }
  tmp_votes_nodes <- xml_find_all(tmp_cand_nodes, "d1:Votes")
  tmp_votes_tbl <- as.data.frame(do.call("rbind", xml_attrs(tmp_votes_nodes)))
  tmp_votes_tbl <- data.frame(tmp_votes_tbl,
                              Votes = xml_text(tmp_votes_nodes),
                              stringsAsFactors = FALSE)

  colnames(tmp_votes_tbl) <- paste(tmp_vote_prefix, colnames(tmp_votes_tbl), sep = ".")
  tmp_df <- data.frame(tmp_df, tmp_votes_tbl, stringsAsFactors = FALSE)
  rownames(tmp_df) <- NULL

  # Columns that should be integers
  tmp_list_ints <- c("DivisionId", tmp_list_ints_type)
  # Columns that should be numeric
  tmp_list_num <- c(tmp_list_num_type)

  if(nrow(tmp_df) == 0) {
    message("Nothing returned.")
  } else {
    tmp_df[tmp_list_ints] <- sapply(tmp_df[tmp_list_ints], as.integer)
    tmp_df[tmp_list_num] <- sapply(tmp_df[tmp_list_num], as.numeric)
  }

  return(tmp_df)
}


get_mediafeed_votes_div_fp <- function(xml) {
  # Note: This is the quick and dirty version with no error checking!
  #
  # This could have elected added pretty easily with:
  # xml_find_all(tmp_cand_nodes, "d1:Elected")
  #
  tmp_div_nodes <- xml_find_all(xml, "d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest")
  tmp_cand_nodes <- xml_find_all(tmp_div_nodes, "d1:PollingDistrictIdentifier|d1:FirstPreferences/d1:Candidate|d1:FirstPreferences/d1:Ghost")

  tmp_df <- data.frame(DivisionId = xml_attr(tmp_cand_nodes, "Id"),
                       CandidateType = xml_name(tmp_cand_nodes),
                       stringsAsFactors = FALSE)
  tmp_df$DivisionId <- Fill(tmp_df$DivisionId)
  tmp_df <- tmp_df[tmp_df$CandidateType != "PollingDistrictIdentifier",]
  tmp_df <- data.frame(tmp_df,
                       CandidateID = xml_attr(xml_find_all(tmp_cand_nodes, "eml:CandidateIdentifier"), "Id"),
                       stringsAsFactors = FALSE)
  tmp_votes_nodes <- xml_find_all(tmp_cand_nodes, "d1:Votes")
  tmp_votes_tbl <- as.data.frame(do.call("rbind", xml_attrs(tmp_votes_nodes)))
  tmp_votes_tbl <- data.frame(tmp_votes_tbl,
                              Votes = xml_text(tmp_votes_nodes),
                              stringsAsFactors = FALSE)
  colnames(tmp_votes_tbl) <- paste("FP", colnames(tmp_votes_tbl), sep = ".")
  tmp_df <- data.frame(tmp_df, tmp_votes_tbl, stringsAsFactors = FALSE)

  # Columns that should be integers
  tmp_list_ints <- c("DivisionId", "CandidateId", "FP.Votes", "FP.Historic", "FP.MatchedHistoric")
  # Columns that should be numeric
  tmp_list_num <- c("FP.Percentage", "FP.Swing")

  tmp_df[tmp_list_ints] <- sapply(tmp_df[tmp_list_ints], as.integer)
  tmp_df[tmp_list_num] <- sapply(tmp_df[tmp_list_num], as.numeric)

  return(tmp_df)

}

get_mediafeed_votes_div2 <- function(xml) {
  # Apparently I wrote this function twice.

  tmp_nodes <- xml_find_all(xml, paste("d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:PollingDistrictIdentifier",
                                       "d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:FirstPreferences/d1:Candidate/eml:CandidateIdentifier",
                                       "d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:FirstPreferences/d1:Ghost/eml:CandidateIdentifier",
                                       sep = "|"))

  tmp_type <- xml_name(tmp_nodes)
  tmp_id <- xml_attr(tmp_nodes, "Id")
  tmp_div_code <- xml_attr(tmp_nodes, "ShortCode")
  # tmp_ind <- xml_attr(tmp_nodes, "Independent")

  tmp_df <- data.frame(tmp_type,
                       DivisionShortCode = tmp_div_code,
                       CandidateId = tmp_id,
                       stringsAsFactors = FALSE)

  tmp_df$DivisionId <- ifelse(tmp_df$tmp_type == "PollingDistrictIdentifier", tmp_df$CandidateId, NA)
  tmp_df$DivisionId <- Fill(tmp_df$DivisionId)
  tmp_df$DivisionShortCode <- Fill(tmp_df$DivisionShortCode)
  tmp_df <- tmp_df[tmp_df$tmp_type == "CandidateIdentifier",]

  tmp_nodes_cand <- xml_find_all(xml, paste("d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:FirstPreferences/d1:Candidate",
                                            "d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:FirstPreferences/d1:Ghost",
                                            sep = "|"))

  if(length(tmp_nodes_cand) != nrow(tmp_df)) {
    stop("Candidates extract is not the same length as candidate and division table.")
  }

  tmp_cand_votes <- as.data.frame(do.call("rbind", xml_attrs(xml_find_first(tmp_nodes_cand, "d1:Votes"))))
  tmp_cand_votes <- data.frame(tmp_cand_votes,
                               Votes = xml_text(xml_find_first(tmp_nodes_cand, "d1:Votes")),
                               stringsAsFactors = FALSE)

  tmp_df <- data.frame(tmp_df, tmp_cand_votes, stringsAsFactors = FALSE)
  tmp_cols_int <- c("CandidateId", "DivisionId", "Historic", "MatchedHistoric", "Votes")
  tmp_cols_num <- c("Percentage", "Swing")

  tmp_df[tmp_cols_int] <- sapply(tmp_df[tmp_cols_int], as.integer)
  tmp_df[tmp_cols_num] <- sapply(tmp_df[tmp_cols_num], as.numeric)

  tmp_df[c("DivisionId", "DivisionShortCode", "CandidateId",
           "Historic", "MatchedHistoric", "Percentage", "Swing", "Votes")]
}

