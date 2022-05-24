#' Extract election data from media feed files
#'
#' @name mediafeed_functions
#'
NULL

#' Get Media Feed Gender
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
    stop("'CandidateList' node note found. Is this a EML 230 candidates file?")
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

#' Get Media Feed Division IDs
#'
#' List division IDs from media feed file.
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

#' Get Division Details from Media Feed
#'
#'
#'
#' @param DivisionID A division ID in either character or interger format.
#' @param xml A pointer to an XML media feed object.
#'
#' @return A \code{data.frame}.
#' @export
#'
#' @examples
#' \dontrun{
#' library(purrr)
#' xml <- read_mediafeed_xml(get_mediafeed_file(2022, "Verbose", Archive = FALSE))
#' map_dfr(get_mediafeed_divisionids(xml), get_mediafeed_division_details, xml)
#' }
#'
#' @importFrom xml2 xml_parent xml_attrs
get_mediafeed_division_details <- function(DivisionID, xml) {

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

  div_df <- data.frame(DivisionID = as.integer(DivisionID),
                       DivisionNm = div_name,
                       t(data.frame(div_enrolment)),
                       t(data.frame(div_fp)),
                       t(data.frame(div_tcp)),
                       stringsAsFactors = FALSE)

  rownames(div_df) <- NULL
  return(div_df)

}

#' Get Media Feed Votes by Polling Place for Division
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

