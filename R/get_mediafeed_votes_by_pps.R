#' Extract votes from the media feed by polling place
#'
#' @name get_mediafeed_votes_by_pps
#'
NULL


#' Get first preference votes by polling place
#'
#' Extract the first preference votes by candidate by polling place from the
#' media feed file, including the historic, percentage and swing.
#'
#' This function is designed to be as quick as minimal as possible whilst still
#' including robust error checking and feedback.
#'
#' The actual results are prefixed with "FP." in order to easily distintinguish
#' them from two-candidate preferred and two-party preferred votes.
#'
#' It assumes that you already have the candidate, divisions and polling place
#' details elsewhere, and will join those by PollingPlaceId and CandidateID. In
#' particular, this function does not include DivisionID or PartyID.
#'
#' This function is quite vociferous because 1) it can be slow, so it tells you
#' what it's doing, and 2) occasionally the media feed changes and this helps
#' you determine where the function has made assumptions that are no longer
#' true.
#'
#' @param xml A pointer to an XML media feed object
#'
#' @return A \code{data.frame} with seven columns: \code{PollingPlaceId},
#'   \code{CandidateType} (one of either \code{Candidate} or \code{Ghost}),
#'   \code{CandidateID}, \code{FP.Historic}, \code{FP.Percentage},
#'   \code{FP.Swing} and \code{FP.Votes}.
#' @export
#'
#' @examples
#' \dontrun{
#' xml <- read_mediafeed_xml(get_mediafeed_file(2022, "Verbose", Archive = TRUE))
#' get_mediafeed_votes_by_pps_fp(xml)}
#'
#' @importFrom xml2 xml_find_all xml_name xml_attr xml_attrs
get_mediafeed_votes_by_pps_fp <- function(xml) {

  message("Extracting nodes... ", appendLF = FALSE)
  tmp_pps_nodes <- xml_find_all(xml, "d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:PollingPlaces/d1:PollingPlace")
  message("Done.")

  # Build the candidates and polling places table
  message("Fetching polling places and candidate types... Extracting nodes... ", appendLF = FALSE)
  tmp_pps_cand_nodes <- xml_find_all(tmp_pps_nodes, "d1:PollingPlaceIdentifier|d1:FirstPreferences/d1:Candidate|d1:FirstPreferences/d1:Ghost")
  message("Fetching polling place IDs... ", appendLF = FALSE)
  tmp_pps_ids <- xml_attr(tmp_pps_cand_nodes, "Id")
  message("Fetching candidate types... ", appendLF = FALSE)
  tmp_cand_types <- xml_name(tmp_pps_cand_nodes)
  if(length(tmp_pps_ids) != length(tmp_cand_types)) {
    stop("Polling place IDs and candidate types not the same lenght.")
  }

  message("Joining them together... ", appendLF = FALSE)
  fp_pps_df <- data.frame(PollingPlaceId = tmp_pps_ids,
                          CandidateType = tmp_cand_types,
                          stringsAsFactors = FALSE)
  message("Done.")

  message("Filling polling place IDs... ", appendLF = FALSE)
  # The Fill function is in utility_functions.R
  fp_pps_df$PollingPlaceId <- Fill(fp_pps_df$PollingPlaceId)
  message("Done.")
  fp_pps_df <- fp_pps_df[fp_pps_df$CandidateType != "PollingPlaceIdentifier",]

  message("Fetching candidate IDs... ", appendLF = FALSE)
  tmp_cand_ids <- xml_attr(xml_find_all(tmp_pps_cand_nodes, "eml:CandidateIdentifier"), "Id")
  if(length(tmp_cand_ids) != nrow(fp_pps_df)) {
    stop("Candidate IDs not the same length as Candidate Types")
  }
  message("Done.")

  fp_pps_df <- data.frame(fp_pps_df,
                          CandidateID = tmp_cand_ids,
                          stringsAsFactors = FALSE)

  # Get the votes
  message("Fetching votes: Historic votes... ", appendLF = FALSE)
  tmp_votes_tbl <- as.data.frame(do.call("rbind", xml_attrs(xml_find_all(tmp_pps_cand_nodes, "d1:Votes"))))
  message("Current votes... ", appendLF = FALSE)
  tmp_votes <- xml_text(xml_find_all(tmp_pps_cand_nodes, "d1:Votes"))
  message("Joing votes ...", appendLF = FALSE)
  if(length(tmp_votes) != nrow(tmp_votes_tbl)) {
    stop("Votes not the same length as votes table")
  }

  if(length(tmp_votes) != nrow(fp_pps_df)) {
    stop("Votes not the same length as candidates & polling place table")
  }

  tmp_votes_tbl <- data.frame(tmp_votes_tbl,
                              Votes = tmp_votes,
                              stringsAsFactors = FALSE)

  colnames(tmp_votes_tbl) <- paste("FP", colnames(tmp_votes_tbl), sep = ".")
  message("Done.")

  fp_pps_df <- data.frame(fp_pps_df,
                          tmp_votes_tbl)

  # Columns that should be integers
  tmp_list_ints <- c("PollingPlaceId", "CandidateID", "FP.Votes", "FP.Historic")
  # Columns that should be numeric
  tmp_list_num <- c("FP.Percentage", "FP.Swing")

  fp_pps_df[tmp_list_ints] <- sapply(fp_pps_df[tmp_list_ints], as.integer)
  fp_pps_df[tmp_list_num] <- sapply(fp_pps_df[tmp_list_num], as.numeric)

  return(fp_pps_df)
}

