#' Extract votes from the media feed by polling place
#'
#' The philosophy of the \code{get_mediafeed_votes_by_pps} functions is to
#' correctly (first priority) and relatively quickly (second priority) extract
#' votes from the media feed by polling place with the minimum amount of
#' extraneous information required to build a results data set.
#'
#' In practice, this means that the outputs of the files contain
#' \code{PollingPlaceID} and (where necessary) \code{CandidateID}, but do not
#' contain division or party information. It is expected that you have already
#' processed the preload (or other complete) files to get this information.
#'
#' Due to the structure of the media feed XML files, getting some of this
#' additional information is extremely slow, and is much slower than just
#' joining on an existing details file.
#'
#' The functions in this family are:
#'
#' \itemize{
#'
#' \item{\code{\link{get_mediafeed_votes_pps_fp}}}{Get first preference votes by polling place}
#'
#' }
#'
#'
#'
#' @name get_mediafeed_votes_pps
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
get_mediafeed_votes_pps_fp <- function(xml) {

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

#' Get two-candidate preferred votes by polling place
#'
#' @param xml A pointer to an XML media feed object
#'
#' @return
#' @export
#'
#' @examples
get_mediafeed_votes_pps_tcp <- function(xml) {
  # This is almost exactly the same as get_mediafeed_votes_by_pps_fp()

  tmp_pps_nodes <- xml_find_all(xml, "d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:PollingPlaces/d1:PollingPlace")
  tmp_pps_cand_nodes <- xml_find_all(tmp_pps_nodes, "d1:PollingPlaceIdentifier|d1:TwoCandidatePreferred/d1:Candidate")
  tmp_pps_ids <- xml_attr(tmp_pps_cand_nodes, "Id")
  tmp_cand_types <- xml_name(tmp_pps_cand_nodes)
  if(length(tmp_pps_ids) != length(tmp_cand_types)) {
    stop("Polling place IDs and candidate types not the same lenght.")
  }

  tcp_pps_df <- data.frame(PollingPlaceId = tmp_pps_ids,
                           CandidateType = tmp_cand_types,
                           stringsAsFactors = FALSE)

  tcp_pps_df$PollingPlaceId <- Fill(tcp_pps_df$PollingPlaceId)
  tcp_pps_df <- tcp_pps_df[tcp_pps_df$CandidateType != "PollingPlaceIdentifier",]

  tmp_cand_ids <- xml_attr(xml_find_all(tmp_pps_cand_nodes, "eml:CandidateIdentifier"), "Id")
  if(length(tmp_cand_ids) != nrow(tcp_pps_df)) {
    stop("Candidate IDs not the same length as Candidate Types")
  }

  tcp_pps_df <- data.frame(tcp_pps_df,
                           CandidateID = tmp_cand_ids,
                           stringsAsFactors = FALSE)
  # Get the votes
  message("Fetching votes: Historic votes... ", appendLF = FALSE)
  tmp_votes_tbl <- as.data.frame(do.call("rbind", xml_attrs(xml_find_all(tmp_pps_cand_nodes, "d1:Votes"))))
  message("Current votes... ", appendLF = FALSE)
  tmp_votes <- xml_text(xml_find_all(tmp_pps_cand_nodes, "d1:Votes"))
  message("Joing votes... ", appendLF = FALSE)
  if(length(tmp_votes) != nrow(tmp_votes_tbl)) {
    stop("Votes not the same length as votes table")
  }

  if(length(tmp_votes) != nrow(tcp_pps_df)) {
    stop("Votes not the same length as candidates & polling place table")
  }

  tmp_votes_tbl <- data.frame(tmp_votes_tbl,
                              Votes = tmp_votes,
                              stringsAsFactors = FALSE)

  colnames(tmp_votes_tbl) <- paste("TCP", colnames(tmp_votes_tbl), sep = ".")
  message("Done.")

  tcp_pps_df <- data.frame(tcp_pps_df,
                           tmp_votes_tbl)

  # The TCP candidates are only added to the file once counting has begun,
  # so they will not be present in some results files, including the preload
  # file.
  if(nrow(tcp_pps_df) == 0) {

    message("Nothing returned.")
    return(tcp_pps_df)

  } else {

    # Columns that should be integers
    tmp_list_ints <- c("PollingPlaceId", "CandidateID", "TCP.Votes", "TCP.Historic")
    # Columns that should be numeric
    tmp_list_num <- c("TCP.Percentage", "TCP.Swing")

    tcp_pps_df[tmp_list_ints] <- sapply(tcp_pps_df[tmp_list_ints], as.integer)
    tcp_pps_df[tmp_list_num] <- sapply(tcp_pps_df[tmp_list_num], as.numeric)

    # Pretty sure we would never need this for TCP.
    tcp_pps_df$CandidateType <- NULL

    return(tcp_pps_df)

  }

}

#' Get total votes by polling place
#'
#' @param xml A pointer to an XML media feed object
#'
#' @return
#' @export
#'
#' @examples
get_mediafeed_votes_pps_total <- function(xml) {
  # This is almost exactly the same as get_mediafeed_fp_by_pps()

  tmp_pps_nodes <- xml_find_all(xml, "d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:PollingPlaces/d1:PollingPlace")
  # tmp_pps_updated <- xml_attr(tmp_pps_nodes, "Updated")
  #
  # When was the polling place result updated
  # xml_attr(xml_find_all(tmp_pps_nodes, "d1:TwoCandidatePreferred"), "Updated")

  tmp_votes_nodes <- xml_find_all(tmp_pps_nodes,
                                  "d1:PollingPlaceIdentifier|d1:FirstPreferences/d1:Formal|d1:FirstPreferences/d1:Informal|d1:FirstPreferences/d1:Total")


  tmp_pps_ids <- xml_attr(tmp_votes_nodes, "Id")
  tmp_vote_types <- xml_name(tmp_votes_nodes)
  if(length(tmp_pps_ids) != length(tmp_vote_types)) {
    stop("Polling place IDs and candidate types not the same lenght.")
  }

  tmp_df <- data.frame(PollingPlaceId = tmp_pps_ids,
                       VoteType = tmp_vote_types,
                       stringsAsFactors = FALSE)

  tmp_df$PollingPlaceId <- Fill(tmp_df$PollingPlaceId)
  tmp_df <- tmp_df[tmp_df$VoteType != "PollingPlaceIdentifier",]

  # tmp_cand_ids <- xml_attr(xml_find_all(tmp_votes_nodes, "eml:CandidateIdentifier"), "Id")
  # if(length(tmp_cand_ids) != nrow(tcp_pps_df)) {
  #   stop("Candidate IDs not the same length as Candidate Types")
  # }
  #
  # tcp_pps_df <- data.frame(tcp_pps_df,
  #                          CandidateID = tmp_cand_ids,
  #                          stringsAsFactors = FALSE)

  # Get the votes
  # message("Fetching votes: Historic votes... ", appendLF = FALSE)
  tmp_votes_tbl <- as.data.frame(do.call("rbind", xml_attrs(xml_find_all(tmp_votes_nodes, "d1:Votes"))))
  # message("Current votes... ", appendLF = FALSE)
  tmp_votes <- xml_text(xml_find_all(tmp_votes_nodes, "d1:Votes"))
  # message("Joing votes ...", appendLF = FALSE)
  if(length(tmp_votes) != nrow(tmp_votes_tbl)) {
    stop("Votes not the same length as votes table")
  }

  if(length(tmp_votes) != nrow(tmp_df)) {
    stop("Votes not the same length as candidates & polling place table")
  }

  tmp_votes_tbl <- data.frame(tmp_votes_tbl,
                              Votes = tmp_votes,
                              stringsAsFactors = FALSE)

  # message("Done.")

  tmp_df <- data.frame(tmp_df,
                       tmp_votes_tbl)

  # Columns that should be integers
  tmp_list_ints <- c("PollingPlaceId", "Historic", "Votes")
  # Columns that should be numeric
  tmp_list_num <- c("Percentage", "Swing")

  tmp_df[tmp_list_ints] <- sapply(tmp_df[tmp_list_ints], as.integer)
  tmp_df[tmp_list_num] <- sapply(tmp_df[tmp_list_num], as.numeric)

  return(tmp_df)

}
