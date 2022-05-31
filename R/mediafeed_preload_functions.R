#' Extract election data from media feed preload files
#'
#' This is a collection of functions that are designed to be run on the preload
#' media feed files (in fact they check that they're running on the preload
#' files because at least one of them depends on a set of nodes that is only in
#' the preload files - the \code{//d1:Analysis} nodes).
#'
#' The intention is that these functions are run on the preload file before the
#' election (or before any other results are analysed) to build tables that can
#' be joined against the results files. Extracting this data is (relatively)
#' slow, and extracting the results is as quick as it can be.
#'
#' The functions included in this family are:
#'
#' \itemize{
#'
#' \item{\code{\link{get_mediafeed_preload_partygroups}}}{Get party groups from
#' preload media feed file}
#'
#' \item{\code{\link{get_mediafeed_preload_candidates}}}{Get candidates from
#' preload media feed file}
#'
#' \item{\code{\link{get_mediafeed_preload_pps}}}{Get polling places from
#' preload media feed file}
#'
#' \item{\code{\link{get_mediafeed_preload_pps_cand}}}{Get candidates and
#' polling places from preload media feed}
#'
#' \item{\code{\link{get_mediafeed_preload_divs}}}{Get divisions from preload
#' media feed file}
#'
#' \item{\code{\link{get_mediafeed_preload_votes_type}}}{Get vote type table
#' from preload media feed file}
#'
#' \item{\code{\link{get_mediafeed_preload_gender}}}{Get candidate gender from
#' preload media feed}
#'
#' }
#'
#' @name mediafeed_preload_functions
#'
NULL

#' Get party groups from preload media feed file
#'
#' Extract a table of party group, IDs and shortcodes from the preload XML file.
#'
#' Note that party groups are not the same as the parties listed in the
#' candidate extract. I'm not sure that there is any way to determine which
#' parties are members of which groups from just the preload files.
#'
#' @param xml A pointer to an XML preload media feed object.
#'
#' @return A \code{data.frame} with three variables: \code{PartyGroupId}
#'   (\code{INT}), \code{PartyGroupShortCode} and \code{PartyGroupName}.
#' @export
#'
#' @examples
#' \dontrun{
#' preload_xml <- read_mediafeed_xml(download_mediafeed_file(2022, "Preload", Archive = TRUE),
#'                                   "results")
#' get_mediafeed_preload_parties(preload_xml)}
#' @importFrom xml2 xml_find_all xml_attrs xml_text
get_mediafeed_preload_partygroups <- function(xml) {

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

#' Get polling places from preload media feed file
#'
#' Get polling places and their associated divisions from the preload media feed
#' XML file.
#'
#' Note that the division name is not included is the output of this function,
#' only the \code{DivisionId} and \code{DivisionShortCode}. To get
#' \code{DivisionNm}, join this file with the output of
#' \code{\link{get_mediafeed_preload_divs}} on \code{DivisionId}.
#'
#' Most of the polling places do not have a \code{PollingPlaceClassification}
#' (that is, they are \code{NA}). As of the 2022 federal election, the
#' classifications are \code{PrePollVotingCentre}, \code{SpecialHospital},
#' \code{PrisonMobile} and \code{RemoteMobile}.
#'
#' The blind and low vision telephone voting (and for 2022 the COVID telephone
#' voting) are classed as \code{PrePollVotingCentre} polling places, but are
#' only identifiable by the \code{PollingPlaceName}.
#'
#' @param xml A pointer to an XML preload media feed object.
#'
#' @return A \code{data.frame} with five variables: \code{DivisionId},
#'   \code{DivisionNm}, \code{DivisionShortCode}, \code{StateAb},
#'   \code{PollingPlaceId}, \code{PollingPlaceName} and
#'   \code{PollingPlaceClassification}.
#' @export
#'
#' @examples
#' preload_xml <- read_mediafeed_xml(download_mediafeed_file(2022, "Preload", Archive = TRUE),
#'                                   "results")
#' get_mediafeed_preload_pps(preload_xml)
#' @importFrom xml2 xml_find_all xml_attrs xml_attr xml_name
get_mediafeed_preload_pps <- function(xml) {

  if(get_mediafeed_metadata(xml)["Phase"] != "Preload") {
    stop("This function can only be used with a preload file.")
  }

  tmp_nodes <- xml_find_all(xml, paste("d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:PollingDistrictIdentifier",
                                       "d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:PollingPlaces/d1:PollingPlace/d1:PollingPlaceIdentifier",
                                       sep = "|"))
  tmp_type <- xml_name(tmp_nodes)
  tmp_id <- xml_attr(tmp_nodes, "Id")
  tmp_div_code <- xml_attr(tmp_nodes, "ShortCode")
  # Slightly more complicated to get division name, but worth the extra effort.
  tmp_div_name <- xml_text(xml_find_first(tmp_nodes, "../d1:PollingDistrictIdentifier/d1:Name"))
  tmp_state <- xml_attr(xml_find_first(tmp_nodes, "../d1:PollingDistrictIdentifier/d1:StateIdentifier"), "Id")
  tmp_pps_name <- xml_attr(tmp_nodes, "Name")
  tmp_pps_clas <- xml_attr(tmp_nodes, "Classification")

  tmp_df <- data.frame(tmp_type,
                       PollingPlaceId = tmp_id,
                       DivisionShortCode = tmp_div_code,
                       DivisionNm = tmp_div_name,
                       StateAb = tmp_state,
                       PollingPlaceNm = tmp_pps_name,
                       PollingPlaceClassification = tmp_pps_clas,
                       stringsAsFactors = FALSE)

  tmp_df$DivisionId <- ifelse(tmp_df$tmp_type == "PollingDistrictIdentifier", tmp_df$PollingPlaceId, NA)
  tmp_df$DivisionId <- Fill(tmp_df$DivisionId)
  tmp_df$DivisionShortCode <- Fill(tmp_df$DivisionShortCode)
  tmp_df$DivisionNm <- Fill(tmp_df$DivisionNm)
  tmp_df$StateAb <- Fill(tmp_df$StateAb)
  tmp_df <- tmp_df[tmp_df$tmp_type == "PollingPlaceIdentifier",]

  # tmp_df$tmp_type <- NULL
  tmp_df[c("DivisionId", "PollingPlaceId")] <- sapply(tmp_df[c("DivisionId", "PollingPlaceId")], as.integer)
  tmp_df[c("DivisionId", "DivisionNm", "DivisionShortCode", "StateAb", "PollingPlaceId", "PollingPlaceNm", "PollingPlaceClassification")]

}

#' Get divisions from preload media feed file
#'
#' Get divisions and enrolments from the preload media feed XML file.
#'
#' This function extracts the divisions and their associated identifiers
#' (including \code{DivisionShortCode}, which only seems to be used in the media
#' feed and nowhere else).
#'
#' Note that \code{\link{get_mediafeed_preload_pps}} includes \code{DivisionId}
#' and \code{DivisionShortName} but not \code{DivisonNm} - this is the only of
#' the preload functions where division name is available, so it makes sense to
#' join this table to the polling place table by \code{DivisionId}.
#'
#' This function also outputs various enrolment numbers for the division.
#' \code{EnrolmentHistoric} can be used for enrolment swing calculations, but
#' it's not immediately clear whether \code{Enrolment} or
#' \code{Enrolment.CloseOfRolls} is the most correct number for calculating
#' turnout.
#'
#' @param xml A pointer to an XML preload media feed object.
#'
#' @return a \code{data.frame} with seven variables: \code{DivisionId},
#'   \code{DivisionShortCode} (a four-character code for the division),
#'   \code{DivisionNm}, \code{StateAb}, \code{Enrolment.CloseOfRolls},
#'   \code{Enrolment.Historic} and \code{Enrolment}
#' @export
#'
#' @examples
#' preload_xml <- read_mediafeed_xml(download_mediafeed_file(2022,
#'                                                           "Preload",
#'                                                           Archive = TRUE),
#'                                   "results")
#'  get_mediafeed_preload_divs(preload_xml)
#' @importFrom xml2 xml_find_all xml_attr xml_attrs
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

#' Get candidates from preload media feed file
#'
#' @param xml A pointer to an XML preload media feed object
#'
#' @return A \code{data.frame} with 22 variables: \code{StateAb},
#'   \code{DivisionId}, \code{DivisionShortCode}, \code{DivisionNm},
#'   \code{CandidateType} (\code{Candidate} or \code{Ghost}), \code{IsGhost}
#'   (\code{logical}), \code{CandidateId}, \code{CandidateNm},
#'   \code{BallotPosition}, \code{IsIndependent} (\code{logical}),
#'   \code{PartyId}, \code{PartyCode}, \code{PartyNm}, \code{Elected}
#'   (\code{logical}), \code{ElectedHistoric} (\code{logical}), \code{Incumbent}
#'   (\code{logical}), \code{IncumbentNotional} (\code{logical}),
#'   \code{FP.Votes}, \code{FP.Percentage}, \code{FP.Swing}, \code{FP.Historic},
#'   \code{FP.MatchedHistoric}.
#' @export
#'
#' @examples
#' preload_xml <- read_mediafeed_xml(download_mediafeed_file(2022,  "Preload", Archive = TRUE),
#'                                   "results")
#' get_mediafeed_preload_candidates(preload_xml)
#' @importFrom xml2 xml_find_all xml_find_first xml_attrs xml_attr xml_text
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
  tmp_div_name <- xml_text(xml_find_first(tmp_nodes, "d1:Name"))
  tmp_div_state <- xml_attr(xml_find_first(tmp_nodes, "d1:StateIdentifier"), "Id")
  # tmp_ind <- xml_attr(tmp_nodes, "Independent")

  tmp_df <- data.frame(tmp_type,
                       StateAb = tmp_div_state,
                       DivisionShortCode = tmp_div_code,
                       DivisionNm = tmp_div_name,
                       CandidateId = tmp_id,
                       stringsAsFactors = FALSE)

  tmp_df$DivisionId <- ifelse(tmp_df$tmp_type == "PollingDistrictIdentifier", tmp_df$CandidateId, NA)
  tmp_df$DivisionId <- Fill(tmp_df$DivisionId)
  tmp_df$StateAb <- Fill(tmp_df$StateAb)
  tmp_df$DivisionShortCode <- Fill(tmp_df$DivisionShortCode)
  tmp_df$DivisionNm <- Fill(tmp_df$DivisionNm)
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
  tmp_cols_nochange <- c("CandidateType", "PartyCode", "PartyNm", "DivisionShortCode", "DivisionNm", "tmp_type", "CandidateNm", "StateAb")
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

  tmp_cand_df[c("StateAb", "DivisionId", "DivisionShortCode", "DivisionNm",
                "CandidateType", "IsGhost", "CandidateId", "CandidateNm",
                "BallotPosition",
                "IsIndependent", "PartyId", "PartyCode", "PartyNm",
                "Elected", "ElectedHistoric", "Incumbent", "IncumbentNotional",
                "FP.Votes", "FP.Percentage", "FP.Swing", "FP.Historic", "FP.MatchedHistoric")]

}

#' Get candidates and polling places from preload media feed
#'
#' Get a \code{data.frame} with candidates by polling place in the same format
#' as the media feed results file.
#'
#' This function is similar to \code{\link{get_mediafeed_preload_pps}} and
#' \code{\link{get_mediafeed_preload_candidates}}, but returns a table of
#' candidates by polling place.
#'
#' The usefulness of this function is that it returns the table of candidates
#' and polling places in exactly the same format and order as
#' \code{\link{get_mediafeed_votes_pps_fp}}, which makes it trivial to merge
#' with the live results file (you don't even have to join it, you could
#' \code{\link{cbind}} it, but you should probably also check that
#' \code{PollingPlaceId} and \code{CandidateId} are equal).
#'
#' @param xml A pointer to an XML preload media feed object
#'
#' @return A \code{data.frame} with the following variables: \code{StateAb},
#'   \code{DivisionId}, \code{DivisionShortCode}, \code{DivisionNm},
#'   \code{PollingPlaceId}, \code{PollingPlaceNm}, \code{CandidateId},
#'   \code{CandidateType}, \code{IsGhost}, \code{Historic}, \code{Percentage},
#'   \code{Swing}, \code{Votes}.
#' @export
#'
#' @examples
#' preload_xml <- read_mediafeed_xml(download_mediafeed_file(2022,  "Preload", Archive = TRUE),
#'                                   "results")
#' get_mediafeed_preload_pps_cand(preload_xml)
#' @importFrom xml2 xml_find_all xml_find_first xml_attrs xml_attr xml_text
get_mediafeed_preload_pps_cand <- function(xml) {
  message("Extracting xml data. This can take a while... ", appendLF = FALSE)
  tmp_nodes <- xml_find_all(xml, paste("d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:PollingDistrictIdentifier",
                                       "d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:PollingPlaces/d1:PollingPlace/d1:PollingPlaceIdentifier",
                                       "d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:PollingPlaces/d1:PollingPlace/d1:FirstPreferences/d1:Candidate",
                                       "d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:PollingPlaces/d1:PollingPlace/d1:FirstPreferences/d1:Ghost",
                                       sep = "|"))

  # # It isn't actually any quickler if you leave off the DivisionId, but it is
  # # less useful
  # tmp_nodes <- xml_find_all(preload_xml, paste("d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:PollingPlaces/d1:PollingPlace/d1:PollingPlaceIdentifier",
  #                                              "d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:PollingPlaces/d1:PollingPlace/d1:FirstPreferences/d1:Candidate",
  #                                              "d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:PollingPlaces/d1:PollingPlace/d1:FirstPreferences/d1:Ghost",
  #                                              sep = "|"))
  message("Done.")

  message("Extracting data:")
  message("Candidate type... ", appendLF = FALSE)
  tmp_type <- xml_name(tmp_nodes)
  message("Candidate ID... ", appendLF = FALSE)
  tmp_id <- xml_attr(tmp_nodes, "Id")
  message("Division codes... ", appendLF = FALSE)
  tmp_div_short <- xml_attr(tmp_nodes, "ShortCode")
  message("Division name... ", appendLF = FALSE)
  tmp_div_name <- xml_text(xml_find_first(tmp_nodes, "d1:Name"))
  message("State... ", appendLF = FALSE)
  tmp_state <- xml_attr(xml_find_first(tmp_nodes, "d1:StateIdentifier"), "Id")
  message("Polling place name... ", appendLF = FALSE)
  tmp_pps_name <- xml_attr(tmp_nodes, "Name")
  # # Can't use Classification
  # tmp_pps_clas <- xml_attr(tmp_nodes, "Classification")
  message("Candidate ID... ", appendLF = FALSE)
  tmp_cand_id <- xml_attr(xml_find_first(tmp_nodes, "eml:CandidateIdentifier"), "Id")
  message("Done.")

  tmp_df <- data.frame(CandidateType = tmp_type,
                       Id = tmp_id,
                       DivisionShortCode  = tmp_div_short,
                       DivisionNm = tmp_div_name,
                       StateAb = tmp_state,
                       PollingPlaceNm = tmp_pps_name,
                       # PollingPlaceClassification = tmp_pps_clas,
                       CandidateId = tmp_cand_id,
                       stringsAsFactors = FALSE)

  tmp_df$DivisionId <- ifelse(tmp_df$CandidateType == "PollingDistrictIdentifier", tmp_df$Id, NA)
  tmp_df[c("Id", "DivisionShortCode", "DivisionNm", "StateAb", "DivisionId")] <- lapply(tmp_df[c("Id", "DivisionShortCode", "DivisionNm", "StateAb", "DivisionId")], Fill)
  tmp_df$PollingPlaceId <- tmp_df$Id
  tmp_df <- tmp_df[tmp_df$CandidateType != "PollingDistrictIdentifier",]
  tmp_df$PollingPlaceNm <- Fill(tmp_df$PollingPlaceNm)
  tmp_df <- tmp_df[tmp_df$CandidateType != "PollingPlaceIdentifier",]

  message("Getting votes... ", appendLF = FALSE)
  tmp_votes <- data.frame(as.data.frame(do.call("rbind", xml_attrs(xml_find_all(tmp_nodes, "d1:Votes")))),
                          Votes = xml_text(xml_find_all(tmp_nodes, "d1:Votes")),
                          stringsAsFactors = FALSE)
  message("Done.")

  if(nrow(tmp_df) != nrow(tmp_votes)) {
    stop("Votes table not the same length as candidates table.")
  }

  tmp_df <- data.frame(tmp_df, tmp_votes, stringsAsFactors = FALSE)
  tmp_col_int <- c("CandidateId", "DivisionId", "PollingPlaceId", "Historic", "Votes")
  tmp_col_num <- c("Percentage", "Swing")

  tmp_df$IsGhost <- ifelse(tmp_df$CandidateType == "Ghost", TRUE, FALSE)

  tmp_df[tmp_col_int] <- sapply(tmp_df[tmp_col_int], as.integer)
  tmp_df[tmp_col_num] <- sapply(tmp_df[tmp_col_num], as.numeric)

  tmp_df[c("StateAb", "DivisionId", "DivisionShortCode", "DivisionNm",
           "PollingPlaceId", "PollingPlaceNm",
           "CandidateId", "CandidateType", "IsGhost",
           "Historic", "Percentage", "Swing", "Votes")]
}

#' Get vote type table from preload media feed file
#'
#' Extract votes by type (Ordinary, Absent, Provisional, PrePoll or Postal) from
#' the media feed.
#'
#' This just calls \code{\link{get_mediafeed_votes_type}} on the preload file
#' for first preference votes, and returns historic votes by type.
#'
#' @param xml A pointer to an XML preload media feed object
#'
#' @return a \code{data.frame}. See \code{\link{get_mediafeed_votes_type}} for
#'   details.
#' @export
#'
#' @examples
#' preload_xml <- read_mediafeed_xml(download_mediafeed_file(2022,  "Preload", Archive = TRUE),
#'                                   "results")
#' get_mediafeed_preload_votes_type(preload_xml)
get_mediafeed_preload_votes_type <- function(xml) {

  if(get_mediafeed_metadata(xml)["Phase"] != "Preload") {
    stop("This function should only be used with a preload file.")
  }

  get_mediafeed_votes_type(xml, count = "fp")
}

#' Get candidate gender from preload media feed
#'
#' Create a \code{data.frame} of candidate IDs and gender from the candidates
#' file in the media feed preload file.
#'
#' Note that the gender information is only found in the
#' \code{eml-230-candidates-(EventId).xml} file, which is in the \code{preload}
#' zip file. If the correct XML node identifying this file
#' (\code{d1:CandidateList}) is not found, the function will halt with an error.
#'
#' The easiest way to get the file is to call \code{\link{read_mediafeed_xml}}
#' with "candidates" as the \code{filename} argument,
#'
#' @param xml A pointer to an XML media feed object.
#' @param chamber One of either \code{House} or \code{Senate}.
#'
#' @return A \code{data.frame} with three columns: \code{CandidateId},
#'   \code{Gender} ("male" or "female"), and \code{EventId}
#' @export
#'
#' @examples
#' \dontrun{
#' file <- download_mediafeed_file(2022, "Preload", "Detailed")
#' xml <- read_mediafeed_xml(file, "candidates")
#' get_mediafeed_preload_gender(xml, chamber = "senate")}
#'
#' @importFrom xml2 read_xml xml_attr xml_find_all xml_find_first xml_text
#'   xml_name
get_mediafeed_preload_gender <- function(xml, chamber) {

  if(is.na(xml_name(xml_find_first(xml, "d1:CandidateList")))) {
    stop("'CandidateList' node not found. Is this a EML 230 candidates file?")
  }

  if(toupper(chamber) == "HOUSE") {
    tmp_chamber <- "H"
  } else if(toupper(chamber) == "SENATE") {
    tmp_chamber <- "S"
  } else {
    stop("chamber must be either 'house' or 'senate'")
  }

  tmp_df <- data.frame(CandidateId = as.integer(xml_attr(xml_find_all(xml,
                                                                      paste0("//d1:ElectionIdentifier[@Id=\"", tmp_chamber, "\"]/../d1:Contest/d1:Candidate/d1:CandidateIdentifier")), "Id")),
                       Gender = xml_text(xml_find_all(xml, paste0("//d1:ElectionIdentifier[@Id=\"", tmp_chamber, "\"]/../d1:Contest/d1:Candidate/d1:Gender"))),
                       stringsAsFactors = FALSE)
  tmp_df$EventId <- as.integer(xml_attr(xml_find_first(xml, "/*/*/d1:EventIdentifier"), "Id"))

  return(tmp_df)

}
