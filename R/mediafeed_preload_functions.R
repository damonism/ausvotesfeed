#' Extract election data from media feed preload files
#'
#' @name mediafeed_preload_functions
#'
NULL

#' Get parties from preload media feed file
#'
#' Extract a table of parties, party IDs and shortcodes from the preload XML
#' file.
#'
#' @param xml A pointer to an XML preload media feed object
#'
#' @return
#' @export A \code{data.frame} with three variables: \code{PartyGroupId}
#'   (\code{INT}), \code{PartyGroupShortCode} and \code{PartyGroupName}.
#'
#' @examples
#' \dontrun{
#' preload_xml <- read_mediafeed_xml(download_mediafeed_file(2022,
#'                                                           "Preload",
#'                                                           Archive = FALSE),
#'                                   "results")
#'  get_mediafeed_preload_parties(preload_xml)}
#'  @importFrom xml2 xml_find_all xml_attrs xml_text
get_mediafeed_preload_parties <- function(xml) {

  if(get_mediafeed_metadata(xml)["Phase"] == "Preload") {

    tmp_party_nodes <- xml_find_all(xml, "d1:Results/d1:Election/d1:House/d1:Analysis/d1:National/d1:FirstPreferences/d1:PartyGroup")

    tmp_df <- data.frame(as.data.frame(do.call("rbind", xml_attrs(xml_find_all(tmp_party_nodes, "d1:PartyGroupIdentifier")))),
                         PartyGroupName = xml_text(xml_find_all(tmp_party_nodes, "d1:PartyGroupIdentifier")),
                         stringsAsFactors = FALSE)

    colnames(tmp_df) <- c("PartyGroupId", "PartyGroupShortCode", "PartyGroupName")
    tmp_df$PartyGroupId <- as.integer(tmp_df$PartyGroupId)

    return(tmp_df)

  } else {
    stop("This function can only be used with a preload file.")
  }
}

get_mediafeed_preload_pps <- function(xml) {

  if(get_mediafeed_metadata(xml)["Phase"] != "Preload") {
    stop("This function can only be used with a preload file.")
  }

  tmp_nodes <- xml_find_all(xml, paste("d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:PollingDistrictIdentifier",
                                       "d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:PollingPlaces/d1:PollingPlace/d1:PollingPlaceIdentifier",
                                       sep = "|"))
  tmp_type <- xml_name(tmp_nodes)
  tmp_id <- xml_attr(tmp_nodes, "Id")
  # Division short code was easier to get out of this query than name, so may as
  # well keep it in the table for convenience sake.
  tmp_div_code <- xml_attr(tmp_nodes, "ShortCode")
  tmp_pps_name <- xml_attr(tmp_nodes, "Name")
  tmp_pps_clas <- xml_attr(tmp_nodes, "Classification")

  tmp_df <- data.frame(tmp_type,
                       PollingPlaceId = tmp_id,
                       DivisionShortCode = tmp_div_code,
                       PollingPlaceName = tmp_pps_name,
                       PollingPlaceClassification = tmp_pps_clas,
                       stringsAsFactors = FALSE)

  tmp_df$DivisionId <- ifelse(tmp_df$tmp_type == "PollingDistrictIdentifier", tmp_df$PollingPlaceId, NA)
  tmp_df$DivisionId <- Fill(tmp_df$DivisionId)
  tmp_df$DivisionShortCode <- Fill(tmp_df$DivisionShortCode)
  tmp_df <- tmp_df[tmp_df$tmp_type == "PollingPlaceIdentifier",]

  # tmp_df$tmp_type <- NULL
  tmp_df[c("DivisionId", "PollingPlaceId")] <- sapply(tmp_df[c("DivisionId", "PollingPlaceId")], as.integer)
  tmp_df[c("DivisionId", "DivisionShortCode", "PollingPlaceId", "PollingPlaceName", "PollingPlaceClassification")]

}

get_mediafeed_preload_divs <- function(xml) {

  if(get_mediafeed_metadata(xml)["Phase"] != "Preload") {
    stop("This function can only be used with a preload file.")
  }

  tmp_nodes <- xml_find_all(xml, "d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:PollingDistrictIdentifier")

  tmp_id <- as.data.frame(do.call("rbind", xml_attrs(tmp_nodes)))
  colnames(tmp_id) <- c("DivisionId", "DivisionShortCode")
  tmp_name <- xml_text(xml_find_all(tmp_nodes, "d1:Name"))
  tmp_state <- xml_attr(xml_find_all(tmp_nodes, "d1:StateIdentifier"), "Id")

  tmp_enrolment_df <- as.data.frame(do.call("rbind", xml_attrs(xml_find_all(tmp_nodes, "../d1:Enrolment"))))
  colnames(tmp_enrolment_df) <- paste("Enrolment", colnames(tmp_enrolment_df), sep = ".")
  tmp_enrolment_df <- data.frame(tmp_enrolment_df,
                                 Enrolment = xml_text(xml_find_all(tmp_nodes, "../d1:Enrolment")),
                                 stringsAsFactors = FALSE)

  tmp_df <- data.frame(tmp_id,
                       DivisionNm = tmp_name,
                       StateAb = tmp_state,
                       tmp_enrolment_df,
                       stringsAsFactors = FALSE)

  tmp_df[c("DivisionId", colnames(tmp_enrolment_df))] <- sapply(tmp_df[c("DivisionId", colnames(tmp_enrolment_df))], as.integer)

  return(tmp_df)
}

get_mediafeed_preload_candidates <- function(xml) {

  if(get_mediafeed_metadata(xml)["Phase"] != "Preload") {
    stop("This function can only be used with a preload file.")
  }

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

  tmp_cand_type <- xml_name(tmp_nodes_cand)
  tmp_cand_ind <- xml_attr(tmp_nodes_cand, "Independent")
  tmp_cand_id <- xml_attr(xml_find_first(tmp_nodes_cand, "eml:CandidateIdentifier"), "Id")
  tmp_cand_name <- xml_text(xml_find_first(tmp_nodes_cand, "eml:CandidateIdentifier/eml:CandidateName"))
  tmp_cand_partyid <- xml_attr(xml_find_first(tmp_nodes_cand, "eml:AffiliationIdentifier"), "Id")
  tmp_cand_partycode <- xml_attr(xml_find_first(tmp_nodes_cand, "eml:AffiliationIdentifier"), "ShortCode")
  tmp_cand_partyname <- xml_text(xml_find_first(tmp_nodes_cand, "eml:AffiliationIdentifier/eml:RegisteredName"))
  tmp_cand_ballot <- xml_text(xml_find_first(tmp_nodes_cand, "d1:BallotPosition"))
  tmp_cand_elected <- xml_text(xml_find_first(tmp_nodes_cand, "d1:Elected"))
  tmp_cand_electedhist <- xml_attr(xml_find_first(tmp_nodes_cand, "d1:Elected"), "Historic")
  tmp_cand_incumbent <- xml_text(xml_find_first(tmp_nodes_cand, "d1:Incumbent"))
  tmp_cand_incumbentnotional <- xml_attr(xml_find_first(tmp_nodes_cand, "d1:Incumbent"), "Notional")

  tmp_cand_votes <- as.data.frame(do.call("rbind", xml_attrs(xml_find_first(tmp_nodes_cand, "d1:Votes"))))
  tmp_cand_votes <- data.frame(tmp_cand_votes,
                               Votes = xml_text(xml_find_first(tmp_nodes_cand, "d1:Votes")),
                               stringsAsFactors = FALSE)
  colnames(tmp_cand_votes) <- paste("FP", colnames(tmp_cand_votes), sep = ".")

  tmp_cand_df <- data.frame(CandidateId2 = tmp_cand_id,
                            CandidateType = tmp_cand_type,
                            IsIndependent = ifelse(is.na(tmp_cand_ind), "false", tmp_cand_ind),
                            CandidateNm = tmp_cand_name,
                            PartyId = tmp_cand_partyid,
                            PartyCode = tmp_cand_partycode,
                            PartyNm = tmp_cand_partyname,
                            BallotPosition = tmp_cand_ballot,
                            Elected = tmp_cand_elected,
                            ElectedHistoric = tmp_cand_electedhist,
                            Incumbent = tmp_cand_incumbent,
                            IncumbentNotional = tmp_cand_incumbentnotional,
                            tmp_cand_votes,
                            stringsAsFactors = FALSE)

  tmp_cand_df <- cbind(tmp_cand_df, tmp_df)

  tmp_cols_logical <- c("Elected", "ElectedHistoric", "Incumbent", "IncumbentNotional", "IsIndependent")
  tmp_cols_nochange <- c("CandidateType", "PartyCode", "PartyNm", "DivisionShortCode", "tmp_type", "CandidateNm")
  tmp_cols_num <- c("FP.Percentage", "FP.Swing")
  tmp_cols_int <- setdiff(colnames(tmp_cand_df), c(tmp_cols_logical, tmp_cols_nochange, tmp_cols_num))

  tmp_cand_df[tmp_cols_logical] <- sapply(tmp_cand_df[tmp_cols_logical], function(x) ifelse(x == "false", FALSE, TRUE))
  tmp_cand_df[tmp_cols_num] <- sapply(tmp_cand_df[tmp_cols_num], as.numeric)
  tmp_cand_df[tmp_cols_int] <- sapply(tmp_cand_df[tmp_cols_int], as.integer)

  # This can be quicker (to type, at least) than filtering on the string "Ghost"
  tmp_cand_df$IsGhost <- ifelse(tmp_cand_df$CandidateType == "Candidate", FALSE, TRUE)

  # Check that the table with the division IDs is in the same order as the candidates table.
  tmp_cand_df$Check <- tmp_cand_df$CandidateId2 == tmp_cand_df$CandidateId
  if(nrow(tmp_cand_df[tmp_cand_df$Check == FALSE,]) != 0) {
    stop("Candidate extract did not match Division extract.")
  }

  tmp_cand_df[c("DivisionId", "DivisionShortCode",
                "CandidateType", "IsGhost", "CandidateId", "CandidateNm",
                "BallotPosition",
                "IsIndependent", "PartyId", "PartyCode", "PartyNm",
                "Elected", "ElectedHistoric", "Incumbent", "IncumbentNotional",
                "FP.Votes", "FP.Percentage", "FP.Swing", "FP.Historic", "FP.MatchedHistoric")]

}
