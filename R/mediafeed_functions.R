#' Extract election data from media feed files
#'
#' @name mediafeed_functions
#'
NULL

#' Get the list of Division IDs from the media feed
#'
#' List House division IDs from media feed file.
#'
#' @param xml A pointer to an XML media feed object.
#'
#' @return A vector of division IDs.
#' @export
#'
#' @examples
#' \dontrun{
#' xml <- read_mediafeed_xml(download_mediafeed_file(2022, "Verbose", Archive = FALSE))
#' get_mediafeed_divisionids(xml)
#' }
get_mediafeed_divisionids <- function(xml) {

  xml_attr(xml_find_all(xml, "//d1:House/d1:Contests/*/eml:ContestIdentifier"), "Id")

}

#' Get House division count status from media feed
#'
#' Return details of the specified division from the media feed file as a
#' single-row \code{data.frame}.
#'
#' @param DivisionID A division ID in either character or integer format.
#' @param xml A pointer to an XML media feed object.
#'
#' @return A \code{data.frame}.
#' @export
#'
#' @examples
#' \dontrun{
#' library(purrr)
#' xml <- read_mediafeed_xml(download_mediafeed_file(2022, "Verbose", Archive = FALSE))
#' map_dfr(get_mediafeed_divisionids(xml), get_mediafeed_division_status, xml)
#' }
#'
#' @importFrom xml2 xml_parent xml_attrs
get_mediafeed_division_status <- function(DivisionID, xml) {

  tmp_nodes <- xml_parent(xml_find_first(xml, paste0("//eml:ContestIdentifier[@Id=\"", DivisionID, "\"]")))
  div_name <- xml_text(xml_find_first(tmp_nodes, "d1:PollingDistrictIdentifier/d1:Name"))
  div_enrolment <- xml_attrs(xml_find_first(tmp_nodes, "d1:Enrolment"))
  if(length(div_enrolment) != 2) {stop("d1:Enrolment returned the wrong number of items")}
  names(div_enrolment) <- paste("Enrolment.", names(div_enrolment), sep = "")
  div_enrolment <- c(div_enrolment,
                     "Enrolment" = xml_text(xml_find_first(tmp_nodes, "d1:Enrolment")))
  div_fp <- xml_attrs(xml_find_first(tmp_nodes, "d1:FirstPreferences"))
  names(div_fp) <- paste("FP.", names(div_fp), sep = "")
  div_tcp <- xml_attrs(xml_find_first(tmp_nodes, "d1:TwoCandidatePreferred"))
  div_tcp["Maverick"] <- ifelse(is.na(div_tcp["Maverick"]), "false", div_tcp["Maverick"])
  names(div_tcp) <- paste("TCP.", names(div_tcp), sep = "")

  div_df <- data.frame(DivisionID = DivisionID,
                       DivisionNm = div_name,
                       t(data.frame(div_enrolment)),
                       t(data.frame(div_fp)),
                       t(data.frame(div_tcp)),
                       stringsAsFactors = FALSE)

  tmp_int <- c("DivisionID", "Enrolment.CloseOfRolls", "Enrolment.Historic", "Enrolment",
               "FP.PollingPlacesReturned", "FP.PollingPlacesExpected",
               "TCP.PollingPlacesReturned", "TCP.PollingPlacesExpected")
  div_df[tmp_int] <- sapply(div_df[tmp_int], as.integer)

  tmp_time <- c("FP.Updated", "TCP.Updated")
  div_df[tmp_time] <- lapply(div_df[tmp_time], as.POSIXct, format = "%FT%T")

  div_df$TCP.Maverick <- ifelse(div_df$TCP.Maverick == "true", TRUE, FALSE)

  rownames(div_df) <- NULL
  return(div_df)

}

#' DEPRECATECD: Get House election candidate details from results media feed
#'
#' List all of the candidates in the results media feed, along with their
#' CandidateID and PartyId, mostly for merging with results by CandidateID.
#'
#' Note that the list of candidates in the results feed is different from the
#' candidates in the candidates file in the preload. The candidates file in the
#' preload includes the candidate's gender, but does not include 'ghost'
#' candidates (candidates who ran in the last election but not this election,
#' included for calculating swings). The results file contains current and ghost
#' candidates, but not some other details like gender.
#'
#' The function can also optionally return the party ID for each candidate,
#' however the data extraction process can be quite slow because it needs to
#' search the XML separately for the party affiliation of each candidate. As
#' such, if you do use this you should plan to run this once and save it, rather
#' than running it each time you do the analysis. But even better, you should
#' have already got this from the preload file.
#'
#' @param xml A pointer to an XML media feed object
#' @param party (\code{LOGICAL}) Fetch party ID (defaults to \code{FALSE}
#'   becuase it is very slow.)
#'
#' @return A \code{data.frame} with 9 variables (10 if \code{party} is
#'   \code{TRUE}).
#'
#' @examples
#' \dontrun{
#' results_xml <- read_mediafeed_xml(download_mediafeed_file(2022, "Verbose", Archive = TRUE))
#' get_mediafeed_fp_by_pps(results_xml)}
get_mediafeed_candidates <- function(xml, party = FALSE) {

  # tmp_cand_nodes <- xml_find_all(results_xml, "d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:FirstPreferences/d1:Candidate")
  tmp_cand_nodes <- xml_find_all(xml, "d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:FirstPreferences/d1:Candidate|d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:FirstPreferences/d1:Ghost")
  tmp_numb_cands <- length(tmp_cand_nodes)

  tmp_cand_type <- xml_name(xml_find_all(tmp_cand_nodes, "../d1:Candidate|../d1:Ghost"))
  if(tmp_numb_cands != length(tmp_cand_type)) { stop("Wrong count of Type.") }

  tmp_cand_ind <- xml_attr(xml_find_all(tmp_cand_nodes, "../d1:Candidate|../d1:Ghost"), "Independent")
  if(tmp_numb_cands != length(tmp_cand_ind)) { stop("Wrong count of Independent.") }

  tmp_cand_id <- xml_attr(xml_find_all(tmp_cand_nodes, "eml:CandidateIdentifier"), "Id")
  if(tmp_numb_cands != length(tmp_cand_id)) { stop("Wrong count of CandidateID.") }

  tmp_cand_name <- xml_text(xml_find_all(tmp_cand_nodes, "*/eml:CandidateName"))
  if(tmp_numb_cands != length(tmp_cand_name)) { stop("Wrong count of CandidateName.") }

  tmp_cand_ballot <- xml_text(xml_find_all(tmp_cand_nodes, "d1:BallotPosition"))
  if(tmp_numb_cands != length(tmp_cand_ballot)) { stop("Wrong count of BallotPosition.") }

  tmp_cand_elected <- xml_text(xml_find_all(tmp_cand_nodes, "d1:Elected"))
  if(tmp_numb_cands != length(tmp_cand_elected)) { stop("Wrong count of Elected.") }

  tmp_cand_elected_hist <- xml_attr(xml_find_all(tmp_cand_nodes, "d1:Elected"), "Historic")
  if(tmp_numb_cands != length(tmp_cand_elected_hist)) { stop("Wrong count of ElectedHistori.") }

  tmp_cand_inc <- xml_text(xml_find_all(tmp_cand_nodes, "d1:Incumbent"))
  if(tmp_numb_cands != length(tmp_cand_inc)) { stop("Wrong count of Notional.") }

  tmp_cand_inc_hist <- xml_attr(xml_find_all(tmp_cand_nodes, "d1:Incumbent"), "Notional")
  if(tmp_numb_cands != length(tmp_cand_inc_hist)) { stop("Wrong count of IncumbentNotional.") }


  cand_df <- data.frame(CandidateID = as.integer(tmp_cand_id),
                        CandidateName = tmp_cand_name,
                        CandidateType = tmp_cand_type,
                        Independent = ifelse(is.na(tmp_cand_ind), FALSE, TRUE),
                        BallotPosition = as.integer(tmp_cand_ballot),
                        Elected = ifelse(tmp_cand_elected == "false", FALSE, TRUE),
                        HistoricElected = ifelse(tmp_cand_elected_hist == "false", FALSE, TRUE),
                        Incumbent = ifelse(tmp_cand_inc == "false", FALSE, TRUE),
                        IncumbentNotional = ifelse(tmp_cand_inc_hist == "false", FALSE, TRUE),
                        stringsAsFactors = FALSE)

  if(party) {
    # This is optional because it's slow. You should really get this from the preload.
    message("Getting party affiliation. This can take a while...")
    tmp_cand_aff_list <- lapply(tmp_cand_id,
                                function(x) xml_attr(xml_find_all(tmp_cand_nodes, paste0("eml:CandidateIdentifier[@Id=\"", x, "\"]/../eml:AffiliationIdentifier")), "Id"))
    tmp_cand_aff <- unlist(lapply(tmp_cand_aff_list,
                                  function(x) if (length(x) == 0) NA else x))
    if(tmp_numb_cands != length(tmp_cand_aff)) { stop("Wrong count of PartyId.") }
    cand_df <- data.frame(cand_df,
                          PartyId = as.integer(tmp_cand_aff))
  }

  return(cand_df)

}

#' Merge candidate details to division results
#'
#' Add candidate and division details to division-level results by
#' \code{CandidateID} and \code{DivisionID}.
#'
#' This function runs \code{\link{get_mediafeed_votes_div}} and merges the
#' results with the output of \code{\link{get_mediafeed_preload_candidates}},
#' which contains all of the candidate names (and details) and division names.
#' In practice, this is a much more useful function for analysis of results than
#' \code{\link{get_mediafeed_votes_div}} by itself.
#'
#' @param preload_cand A \code{data.frame} generated by
#'   \code{\link{get_mediafeed_preload_candidates}}.
#' @param xml A pointer to an XML media feed object.
#' @param type Either "fp" (first preference) or "tcp" (two-candidate
#'   preferred).
#'
#' @return A \code{data.frame}
#' @export
#'
#' @examples
#' \dontrun{
#' preload_xml <- read_mediafeed_xml(download_mediafeed_file(2022, "Preload", Archive = TRUE),
#'                                   "results")
#'
#' mf_cand <- get_mediafeed_preload_candidates(preload_xml)
#'
#' results_xml <- read_mediafeed_xml(download_mediafeed_file(2022, "Verbose", Archive = TRUE))
#'
#' merge_div_candidates(mf_cand, results_xml, "fp")}
#'
#' @seealso \code{\link{get_mediafeed_votes_div}} for the details of the
#'   function that actually fetches the results.
merge_div_candidates <- function(preload_cand, xml, type) {

  tmp_results <- get_mediafeed_votes_div(xml, type)
  if("CandidateType" %in% colnames(tmp_results)) {
    tmp_results$CandidateType <- NULL
  }

  tmp_cand <- preload_cand
  tmp_cand[c("FP.Historic", "FP.Percentage", "FP.Swing", "FP.Votes", "FP.MatchedHistoric")] <- NULL

  if("DivisionID" %in% colnames(tmp_results)) {

    tmp_join_cols <- c("DivisionID", "CandidateID")

    # # If the preload file is the polling place file, reduce it to the division file
    # if(length(setdiff(c("PollingPlaceID", "PollingPlaceNm"), colnames(tmp_cand))) == 0) {
    #   tmp_cand[c("PollingPlaceID", "PollingPlaceNm")] <- NULL
    #   tmp_cand <- unique(tmp_cand)
    #
    #   # # This won't work if it's a TCP results file
    #   # if(nrow(tmp_cand) != nrow(tmp_results)) {
    #   #   stop("Results and preload candidates file different lengths.")
    #   # }
    #
    # }


  } else if ("PollingPlaceID" %in% colnames(tmp_results)) {

    tmp_join_cols <- c("PollingPlaceID", "CandidateID")

  }

  if(length(setdiff(tmp_join_cols, colnames(tmp_cand))) > 0) {
    stop("Cannot find CandidateID and DivisionID in preload input")
  }

  tmp_results$.order <- 1:nrow(tmp_results)

  tmp_merged <- merge(tmp_results, tmp_cand, by = tmp_join_cols, all.x = TRUE)
  tmp_merged <- tmp_merged[match(tmp_results$.order, tmp_merged$.order),]
  tmp_merged$.order <- NULL
  rownames(tmp_merged) <- NULL

  return(tmp_merged)

}

# get_mediafeed_votes_candidates_fp <- function(xml) {
#   tmp_cand_nodes <- xml_find_all(xml, "d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:FirstPreferences/d1:Candidate|d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:FirstPreferences/d1:Ghost")
#   tmp_cand_id <- xml_attr(xml_find_all(tmp_cand_nodes, "eml:CandidateIdentifier"), "Id")
#   tmp_cand_votes_types <- xml_attrs(xml_find_all(tmp_cand_nodes, "d1:Votes"))
#   tmp_cand_votes_curr <- xml_text(xml_find_all(tmp_cand_nodes, "d1:Votes"))
#
#   tmp_df <- as.data.frame(do.call("rbind", tmp_cand_votes_types))
#   tmp_df$Votes <- tmp_cand_votes_curr
#   tmp_df$CandidateID <- tmp_cand_id
#
#   tmp_df <- as.data.frame((sapply(tmp_df, as.numeric)))
#   return(tmp_df)
# }

#' DEPRECATED: Get votes by polling place for division from media feed
#'
#' Extract the votes by candidate and polling place for a division from the
#' media feed.
#'
#' Note: The different vote types are \code{Candidate} (normal candidate votes),
#' \code{Ghost} (Candidates from previous elections not running in this
#' election, included for swing purpose), \code{Formal} (total formal votes),
#' \code{Informal}, \code{TotalVotes} and \code{TCP Votes} (two candidate
#' preferred votes).
#'
#' @param DivisionID A division ID in either character or interger format.
#' @param xml A pointer to an XML media feed object.
#'
#' @return a \code{data.frame} with seven columns: \code{DivisionID},
#'   \code{PollingPlaceID}, \code{CandidateID}, \code{CandidateType},
#'   \code{Which} and \code{EventIdentifier}.
#'
#' @examples
#' \dontrun{
#' library(purrr)
#' xml <- read_mediafeed_xml(download_mediafeed_file(2022, "Verbose", Archive = FALSE))
#' map_dfr(get_mediafeed_divisionids(xml), get_mediafeed_votes_pps, xml)
#' }
get_mediafeed_votes_pps <- function(DivisionID, xml) {

  tmp_eventid <- xml_attr(xml_find_first(xml, "//eml:EventIdentifier"), "Id")

  tmp_root <- paste0("//d1:House/d1:Contests/*/eml:ContestIdentifier[@Id=",
                     DivisionID,
                     "]/../d1:PollingPlaces/d1:PollingPlace")

  tmp_pps <- xml_attr(xml_find_all(xml, paste0(tmp_root, "/d1:PollingPlaceIDentifier")), "Id")
  tmp_cand <- xml_attr(xml_find_all(xml, paste0(tmp_root, "/*/*/eml:CandidateIdentifier")), "Id")
  tmp_votes <- xml_text(xml_find_all(xml, paste0(tmp_root, "/*/*/d1:Votes")))
  tmp_historic <- xml_attr(xml_find_all(xml, paste0(tmp_root, "/*/*/d1:Votes")), "Historic")
  tmp_swing <- xml_attr(xml_find_all(xml, paste0(tmp_root, "/*/*/d1:Votes")), "Swing")
  tmp_type <- xml_name(xml_find_all(xml, paste0(tmp_root, "/d1:FirstPreferences/*")))

  tmp_cand_numb <- length(tmp_cand) / length(tmp_pps)
  # message(tmp_cand_numb)
  tmp_fp_cand <- tmp_cand[1:(tmp_cand_numb - 2)]
  tmp_tcp_cand <- tmp_cand[(tmp_cand_numb - 1):tmp_cand_numb]
  tmp_cand_list <- c(tmp_fp_cand, c(NA, NA, NA), tmp_tcp_cand)
  tmp_cand_type <- c(tmp_type[1:(length(tmp_type) / length(tmp_pps))], "TCP", "TCP")
  # message(tmp_cand_type)
  # tmp_cand_type <- c(rep(NA, length(tmp_fp_cand)), c("Formal", "Informal", "Total"), c("TCP", "TCP"))

  tmp_df <- data.frame(DivisionID = DivisionID,
             PollingPlaceID = rep(tmp_pps, each = tmp_cand_numb + 3),
             CandidateID = rep(tmp_cand_list, times = length(tmp_pps)),
             CandidateType = rep(tmp_cand_type, times = length(tmp_pps)),
             stringsAsFactors = FALSE)

  tmp_return <- rbind(data.frame(tmp_df,
                                 Which = "Votes",
                                 Votes = tmp_votes,
                                 stringsAsFactors = FALSE),
                      data.frame(tmp_df,
                                 Which = "Historic",
                                 Votes = tmp_historic,
                                 stringsAsFactors = FALSE),
                      data.frame(tmp_df,
                                 Which = "Swing",
                                 Votes = tmp_swing,
                                 stringsAsFactors = FALSE))

  tmp_return$EventIdentifier <- as.integer(tmp_eventid)

  return(tmp_return)

}

#' Get votes by type from the media feed
#'
#' Extract votes by type (Ordinary, Absent, Provisional, PrePoll or Postal) from
#' the media feed.
#'
#' All of the vote types except \code{Ordinary} votes are types of declaration
#' votes.
#'
#' It is important to note that the \code{PrePoll} votes returned by this
#' function are pre-poll declaration votes, which are pre-poll votes cast
#' outside the elector's division. Pre-poll ordinary votes are what most people
#' mean by "pre-poll votes". These can be found through the
#' \code{\link{get_mediafeed_preload_pps}} function, and have a
#' \code{PollingPlaceClassification} of \code{PrePollVotingCentre}.
#'
#' Note that for maximum extract speed the data is extracted in the tabular
#' format that it appears in the media feed XML file, which is probably not
#' optimal for most analysis. For best results, the resulting \code{data.frame}
#' should probably be pivoted into a \code{tidyr} "long" format.
#'
#' @param xml A pointer to an XML media feed object.
#' @param count Currently \code{fp} for first preferences or \code{tcp} for
#'   two-candidate preferred.
#'
#' @return a \code{data.frame} with seven variables: \code{CandidateID},
#'   \code{DivisionID}, \code{Type} (\code{Ordinary}, \code{Absent},
#'   \code{Provisional}, \code{PrePoll}, \code{Postal}), \code{Historic},
#'   \code{Percentage}, \code{Swing} and \code{Votes}.
#' @export
#'
#' @examples
#' \dontrun{
#' results_xml <- read_mediafeed_xml(download_mediafeed_file(2022, "Verbose", Archive = TRUE))
#' get_mediafeed_votes_type(results_xml, count = "fp")}
#'
#' @importFrom xml2 xml_find_all xml_attr xml_attrs xml_name xml_text
get_mediafeed_votes_type <- function(xml, count = "fp") {
  # NOTE: I think this is the most efficient strategy to get the results data
  #       and I will probably convert the other functions over to this approach
  #       when I get a chance. (DM 2022-05-28)

  tmp_metadata <- get_mediafeed_metadata(xml, short = FALSE)

  if(tolower(count) == "fp") {

    tmp_path <- paste("d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:PollingDistrictIdentifier",
                      "d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:FirstPreferences/d1:Candidate/eml:CandidateIdentifier",
                      "d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:FirstPreferences/d1:Candidate/d1:VotesByType/d1:Votes",
                      "d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:FirstPreferences/d1:Ghost/eml:CandidateIdentifier",
                      "d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:FirstPreferences/d1:Ghost/d1:VotesByType/d1:Votes",
                      sep = "|")

  } else if(tolower(count) == "tcp") {

    if(tmp_metadata["Phase"] == "Preload") {
      stop("TCP information is not present in the Preload file.")
    }

    tmp_path <- paste("d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:PollingDistrictIdentifier",
                      "d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:TwoCandidatePreferred/d1:Candidate/eml:CandidateIdentifier",
                      "d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:TwoCandidatePreferred/d1:Candidate/d1:VotesByType/d1:Votes",
                      "d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:TwoCandidatePreferred/d1:Ghost/eml:CandidateIdentifier",
                      "d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:TwoCandidatePreferred/d1:Ghost/d1:VotesByType/d1:Votes",
                      sep = "|")

  } else if(tolower(count) == "tpp") {
    stop("TPP is currently not implemented")
    tmp_path <- paste("d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:PollingDistrictIdentifier",
                      "d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:TwoPartyPreferred/d1:Coalition/d1:CoalitionIdentifier",
                      "d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:TwoPartyPreferred/d1:Coalition/d1:CoalitionIdentifier/d1:VotesByType/d1:Votes",
                      sep = "|")

  } else {
    stop("count must be one of 'fp', 'tcp' or 'tpp'.")
  }

  tmp_votes_nodes <- xml_find_all(xml, tmp_path)

  tmp_votes <- data.frame(as.data.frame(do.call("rbind", xml_attrs(tmp_votes_nodes))),
                          Votes = xml_text(tmp_votes_nodes),
                          stringsAsFactors = FALSE)

  tmp_df <- data.frame(Id = xml_attr(tmp_votes_nodes, "Id"),
                       Name = xml_name(tmp_votes_nodes),
                       tmp_votes,
                       stringsAsFactors = FALSE)

  tmp_df$DivisionID <- ifelse(tmp_df$Name == "PollingDistrictIdentifier", tmp_df$Id, NA)
  tmp_df$DivisionID <- Fill(tmp_df$DivisionID)
  tmp_df <- tmp_df[tmp_df$Name != "PollingDistrictIdentifier",]
  # CandidateID needs to be done after DivisionID
  tmp_df$CandidateID <- ifelse(tmp_df$Name == "CandidateIdentifier", tmp_df$Id, NA)
  tmp_df$CandidateID <- Fill(tmp_df$CandidateID)
  tmp_df <- tmp_df[tmp_df$Name != "CandidateIdentifier",]

  tmp_df <- tmp_df[c("CandidateID", "DivisionID", colnames(tmp_votes))]
  tmp_df[c("CandidateID", "DivisionID", "Historic", "Votes")] <- sapply(tmp_df[c("CandidateID", "DivisionID", "Historic", "Votes")], as.integer)
  tmp_df[c("Percentage", "Swing")] <- sapply(tmp_df[c("Percentage", "Swing")], as.numeric)

  return(tmp_df)
}

#' Fetch election results media feed
#'
#' Fetch the current media feed results xml for the nominated election.
#'
#' This is simply a shortcut for \code{\link{download_mediafeed_file}} and
#' \code{\link{read_mediafeed_xml}}.
#'
#' @param election AEC EventIdentifier or election year
#' @param api If \code{FALSE} (default), download the file from the AEC's FTP
#'   site, otherwise, the URL "including port and trailing slash" of the server
#'   running the media feed API.
#' @param archive If true, use the media feed archive site, if \code{FALSE}
#'   (default), use the live results media feed (only available during an
#'   election).
#'
#' @return An XML media feed object.
#' @export
#'
#' @examples
#' \dontrun{resuls_xml <- fetch_mediafeed_results(2022)}
fetch_mediafeed_results <- function(election, api = FALSE, archive = FALSE) {

  if(api != FALSE) {

    tmp_file <- download_mediafeed_api(Server = api,
                           EventIdentifier = election,
                           Filetype = "Verbose",
                           Archive = archive)

  } else {

    tmp_file <- download_mediafeed_file(EventIdentifier = election,
                                        Filetype = "Verbose",
                                        Archive = archive)
  }

  read_mediafeed_xml(tmp_file, filename = "results")

}

#' Get national media feed analysis
#'
#' Get the AEC's aggregate results by party group nationally.
#'
#' @param xml An XML media feed object.
#'
#' @return A \code{data.frame} with seven variables: \code{CandidateType} (one
#'   of \code{PartyGroup}, \code{AmalgamatedGhostGroups}, \code{Formal},
#'   \code{Informal} or \code{Total}), \code{PartyGroupID},
#'   \code{PartyGroupCode}, \code{PartyGroupNm}, \code{Percentage}, \code{Swing}
#'   and \code{Votes}.
#' @export
#'
#' @examples
#' results_xml <- read_mediafeed_xml(download_mediafeed_file(2022, "Verbose", Archive = TRUE))
#' get_mediafeed_analysis(results_xml)
#'
#' @seealso \code{\link{get_mediafeed_analysis_states}} for the same results
#'   disaggregated by state.
#' @importFrom xml2 xml_find_all xml_find_first xml_name xml_attr xml_text
get_mediafeed_analysis <- function(xml) {
  tmp_nodes <- xml_find_all(xml, "/d1:MediaFeed/d1:Results/d1:Election/d1:House/d1:Analysis/d1:National/d1:FirstPreferences/*")

  tmp_df <- data.frame(CandidateType = xml_name(tmp_nodes),
             PartyGroupID = xml_attr(xml_find_first(tmp_nodes, "d1:PartyGroupIDentifier"), "Id"),
             PartyGroupCode = xml_attr(xml_find_first(tmp_nodes, "d1:PartyGroupIDentifier"), "ShortCode"),
             PartyGroupNm = xml_text(xml_find_first(tmp_nodes, "d1:PartyGroupIDentifier/d1:PartyGroupName")),
             Percentage = xml_attr(xml_find_first(tmp_nodes, "d1:Votes"), "Percentage"),
             Swing = xml_attr(xml_find_first(tmp_nodes, "d1:Votes"), "Swing"),
             Votes = xml_text(xml_find_first(tmp_nodes, "d1:Votes")),
             stringsAsFactors = FALSE)

  tmp_df$PartyGroupID <- as.integer(tmp_df$PartyGroupID)
  tmp_df$Votes <- as.integer(tmp_df$Votes)
  tmp_df$Percentage <- as.numeric(tmp_df$Percentage)
  tmp_df$Swing <- as.numeric(tmp_df$Swing)

  return(tmp_df)

}

#' Get media feed analysis by state
#'
#' Get the AEC's aggregate results by party group by state.
#'
#' @param xml An XML media feed object
#'
#' @return A \code{data.frame} with eight variables: \code{StateAb},
#'   \code{CandidateType} (one of \code{PartyGroup},
#'   \code{AmalgamatedGhostGroups}, \code{Formal}, \code{Informal} or
#'   \code{Total}), \code{PartyGroupID}, \code{PartyGroupCode},
#'   \code{PartyGroupNm}, \code{Percentage}, \code{Swing} and \code{Votes}.
#' @export
#'
#' @examples
#' results_xml <- read_mediafeed_xml(download_mediafeed_file(2022, "Verbose", Archive = TRUE))
#' get_mediafeed_analysis_states(results_xml)
#'
#' @seealso \code{\link{get_mediafeed_analysis}} for the same results as a
#'   national total.
#' @importFrom xml2 xml_find_all xml_find_first xml_name xml_attr xml_text
get_mediafeed_analysis_states <- function(xml) {
  tmp_nodes <- xml_find_all(xml, "/d1:MediaFeed/d1:Results/d1:Election/d1:House/d1:Analysis/d1:States/d1:State/d1:StateIdentifier|/d1:MediaFeed/d1:Results/d1:Election/d1:House/d1:Analysis/d1:States/d1:State/d1:FirstPreferences/*")

  tmp_df <- data.frame(CandidateType = xml_name(tmp_nodes),
                       StateAb = xml_attr(tmp_nodes, "Id"),
                       PartyGroupID = xml_attr(xml_find_first(tmp_nodes, "d1:PartyGroupIDentifier"), "Id"),
                       PartyGroupCode = xml_attr(xml_find_first(tmp_nodes, "d1:PartyGroupIDentifier"), "ShortCode"),
                       PartyGroupNm = xml_text(xml_find_first(tmp_nodes, "d1:PartyGroupIDentifier/d1:PartyGroupName")),
                       Percentage = xml_attr(xml_find_first(tmp_nodes, "d1:Votes"), "Percentage"),
                       Swing = xml_attr(xml_find_first(tmp_nodes, "d1:Votes"), "Swing"),
                       Votes = xml_text(xml_find_first(tmp_nodes, "d1:Votes")),
                       stringsAsFactors = FALSE)

  tmp_df$StateAb <- Fill(tmp_df$StateAb)
  tmp_df <- tmp_df[tmp_df$CandidateType != "StateIdentifier",]

  tmp_df$PartyGroupID <- as.integer(tmp_df$PartyGroupID)
  tmp_df$Votes <- as.integer(tmp_df$Votes)
  tmp_df$Percentage <- as.numeric(tmp_df$Percentage)
  tmp_df$Swing <- as.numeric(tmp_df$Swing)

  return(tmp_df[c("StateAb", "CandidateType", "PartyGroupID", "PartyGroupCode", "PartyGroupNm", "Percentage", "Swing", "Votes")])

}
