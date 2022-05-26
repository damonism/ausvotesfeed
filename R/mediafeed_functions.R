#' Extract election data from media feed files
#'
#' @name mediafeed_functions
#'
NULL

#' Get candidate gender from media feed
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
#' get_mediafeed_gender(xml, chamber = "senate")
#' }
#'
#' @importFrom xml2 read_xml xml_attr xml_find_all xml_find_first xml_text
#'   xml_name
get_mediafeed_gender <- function(xml, chamber) {

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
#' xml <- read_mediafeed_xml(get_mediafeed_file(2022, "Verbose", Archive = FALSE))
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
#' xml <- read_mediafeed_xml(get_mediafeed_file(2022, "Verbose", Archive = FALSE))
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

#' Get House election candidate details from results media feed
#'
#' List all of the candidates in the results media feed, along with their
#' CandidateId and PartyId, mostly for merging with results by CandidateId.
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
#' @export
#'
#' @examples
#' \dontrun{
#' results_xml <- read_mediafeed_xml(get_mediafeed_file(2022, "Verbose", Archive = TRUE))
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
  if(tmp_numb_cands != length(tmp_cand_id)) { stop("Wrong count of CandidateId.") }

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


  cand_df <- data.frame(CandidateId = as.integer(tmp_cand_id),
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
#' get_mediafeed_fp_by_pps(xml)}
#'
#' @importFrom xml2 xml_find_all xml_name xml_attr xml_attrs
get_mediafeed_fp_by_pps <- function(xml) {

  message("Extracting nodes... ", appendLF = FALSE)
  tmp_pps_nodes <- xml_find_all(results_xml, "d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:PollingPlaces/d1:PollingPlace")
  message("Done.")

  # Build the candidates and polling places table
  message("Fetching polling places and candidate types... Extracting nodes... ", appendLF = FALSE)
  tmp_pps_cand_nodes <- xml_find_all(tmp_pps_nodes, "d1:PollingPlaceIdentifier|d1:FirstPreferences/d1:Candidate|d1:FirstPreferences/d1:Ghost")
  message("Fetching candidate IDs... ", appendLF = FALSE)
  tmp_pps_ids <- xml_attr(tmp_pps_cand_nodes, "Id")
  message("Fetching candidate types... ", appendLF = FALSE)
  tmp_cand_types <- xml_name(tmp_pps_cand_nodes)
  if(length(tmp_pps_ids) != length(tmp_cand_types)) {
    stop("Polling Place IDs and Candidate Types Not the Same Lenght.")
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

  tmp_cand_ids <- xml_attr(xml_find_all(tmp_pps_cand_nodes, "eml:CandidateIdentifier"), "Id")
  if(length(tmp_cand_ids) != nrow(fp_pps_df)) {
    stop("Candidate IDs not the same length as Candidate Types")
  }

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


# get_mediafeed_votes_candidates_fp <- function(xml) {
#   tmp_cand_nodes <- xml_find_all(xml, "d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:FirstPreferences/d1:Candidate|d1:Results/d1:Election/d1:House/d1:Contests/d1:Contest/d1:FirstPreferences/d1:Ghost")
#   tmp_cand_id <- xml_attr(xml_find_all(tmp_cand_nodes, "eml:CandidateIdentifier"), "Id")
#   tmp_cand_votes_types <- xml_attrs(xml_find_all(tmp_cand_nodes, "d1:Votes"))
#   tmp_cand_votes_curr <- xml_text(xml_find_all(tmp_cand_nodes, "d1:Votes"))
#
#   tmp_df <- as.data.frame(do.call("rbind", tmp_cand_votes_types))
#   tmp_df$Votes <- tmp_cand_votes_curr
#   tmp_df$CandidateId <- tmp_cand_id
#
#   tmp_df <- as.data.frame((sapply(tmp_df, as.numeric)))
#   return(tmp_df)
# }

#' Get votes by polling place for division from media feed
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
#' @export
#'
#' @examples
#' \dontrun{
#' library(purrr)
#' xml <- read_mediafeed_xml(get_mediafeed_file(2022, "Verbose", Archive = FALSE))
#' map_dfr(get_mediafeed_divisionids(xml), get_mediafeed_votes_pps, xml)
#' }
get_mediafeed_votes_pps <- function(DivisionID, xml) {

  tmp_eventid <- xml_attr(xml_find_first(xml, "//eml:EventIdentifier"), "Id")

  tmp_root <- paste0("//d1:House/d1:Contests/*/eml:ContestIdentifier[@Id=",
                     DivisionID,
                     "]/../d1:PollingPlaces/d1:PollingPlace")

  tmp_pps <- xml_attr(xml_find_all(xml, paste0(tmp_root, "/d1:PollingPlaceIdentifier")), "Id")
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

