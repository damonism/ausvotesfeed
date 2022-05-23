#' Download file from AEC media feed FTP site
#'
#' Downloads a specific files from the AEC's media feed FTP site
#' and returns the filename and path of the file.
#'
#' @param EventIdentifier AEC event identifier or election year
#' @param Filetype One of \code{Light}, \code{Preload}, \code{PollingDistricts} or \code{Verbose}
#' @param Detail One of either \code{Detailed} (default) or \code{Standard}
#' @param Dest Destination path for the downloaded file (will be created if it does not exist) and defaults to \code{tempdir()}
#' @param Archive If \code{TRUE} (default), use the AEC's media feed archive site, or \code{FALSE} for the live election results
#'
#' @return A file path of the downloaded file
#' @export
#'
#' @examples
#' \dontrun{
#' download_mediafeed_file(2022, "Preload", "Detailed", "/tmp")
#' }
#'
#' @seealso \link{read_mediafeed_xml} for reading the resulting file.
#'
#' @importFrom curl curl curl_download
download_mediafeed_file <- function(EventIdentifier, Filetype, Detail = "Detailed", Dest = tempdir(), Archive = TRUE) {

  event_id <- vtr_identifier(EventIdentifier, as = "event")

  if(!Filetype %in% c("Light", "Preload", "PollingDistricts", "Verbose")) {
    stop(Filetype, " is not a valid Filetype")
  }

  if(!Detail %in% c("Detailed", "Standard")) {
    stop(Filetype, " is not a valid Filetype")
  }

  host <- ifelse(Archive, "mediafeedarchive", "mediafeed")

  path <- paste0("ftp://", host, ".aec.gov.au/",
                 event_id, "/", Detail, "/", Filetype, "/")

  dir <- readLines(curl(path, "r"))
  filename <- rev(strsplit(tail(dir, 1), " +")[[1]])[1]

  if(!dir.exists(Dest)) {
    dir.create(Dest)
  }

  curl_download(paste0(path, filename), paste0(Dest, "/", filename), quiet = FALSE, mode = "wb")

  return(paste0(Dest, "/", filename))
}

#' Read AEC media feed XML file
#'
#' Read the media feed XML file and return an \code{xml2} pointer, unzipping the file if necessary.
#'
#' This is either a very thin wrapper around \code{xml2::read_xml()} or an easy
#' way to access a media feed XML file that is contained in a zip file, such as
#' one downloaded by \link{get_mediafeed_file}.
#'
#' @param path Full path to zip file or XML file
#' @param filename In the case of the preload zip file, one of \code{results},
#'   \code{pollingdistricts}, \code{event} or \code{candidates}
#'
#' @return An \code{XML2} library XML pointer
#' @export
#'
#' @examples
#' \dontrun{
#' # Read a file that you have downloaded yourself
#' read_mediafeed_xml("/tmp/aec-mediafeed-Detailed-Preload-27966-20220503123540.zip", "results")
#'
#' # Read a file that has been downloaded with read_mediafeed_xml()
#' read_mediafeed_xml(download_mediafeed_file(2022, "Preload", "Detailed", "/tmp"))
#' }
#'
#' @seealso \code{\link{download_mediafeed_file}} for downloading the correct
#'   file from the AEC's FTP site.
#'
#' @importFrom xml2 read_xml
#' @importFrom utils unzip
read_mediafeed_xml <- function(path, filename = NA) {

  filenames <- c("results", "pollingdistricts", "event", "candidates")
  if(!is.na(filename)) {
    if(!filename %in% filenames) {
      stop(filename, " is not a valid filename.")
    }
  }

  filetype <- rev(strsplit(path, ".", fixed = TRUE)[[1]])[1]
  # message(filetype)

  vtr_id <- grep("^[0-9]{5}$", strsplit(rev(strsplit(path, "/", fixed = TRUE)[[1]])[1], "-", fixed = TRUE)[[1]], value = TRUE)

  if(filetype == "xml") {
    return(xml2::read_xml(path))
  }

  if(filetype == "zip") {
    unzip(path, overwrite = TRUE, junkpaths = TRUE, exdir = tempdir())
    if(!is.na(filename)) {
      tmp_xml_file <- list.files(tempdir(), pattern = paste0(filename, ".*\\.xml"), full.names = TRUE)
    } else {
      tmp_xml_file <- list.files(tempdir(), pattern = paste0("aec-mediafeed-results-.*", vtr_id, "\\.xml"), full.names = TRUE)
    }
    # message(tmp_xml_file)
    return(xml2::read_xml(tmp_xml_file))
  }
}

#' Get Media Feed Gender
#'
#' Create a \code{data.frame} of candidate IDs and gender from the candidates
#' file in the media feed preload file.
#'
#' Note that the gender information is only found in the
#' \code{eml-230-candidates-(EventId).xml} file, which is in the \code{preload}
#' zip file.
#'
#' The easiest way to get the file is to call \code{link{read_mediafeed_xml}}
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
#' @importFrom xml2 read_xml xml_attr xml_find_all xml_find_first
get_mediafeed_gender <- function(xml, chamber) {

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

get_mediafeed_candidates <- function(DivisionID, xml) {

  tmp_candidates <- data.frame()

  tmp_cands <- xml_attr(xml_find_all(xml, paste0("//d1:House/d1:Contests/*/eml:ContestIdentifier[@Id=",
                                                       DivisionID
                                                       ,"]/../d1:FirstPreferences/d1:Candidate/eml:CandidateIdentifier")), "Id")
  tmp_ghosts <- xml_attr(xml_find_all(xml, paste0("//d1:House/d1:Contests/*/eml:ContestIdentifier[@Id=",
                                                        DivisionID,
                                                        "]/../d1:FirstPreferences/d1:Ghost/eml:CandidateIdentifier")), "Id")

  tmp_tcp <- xml_attr(xml_find_all(xml, paste0("//d1:House/d1:Contests/*/eml:ContestIdentifier[@Id=",
                                                     DivisionID,
                                                     "]/../d1:TwoCandidatePreferred/d1:Candidate/eml:CandidateIdentifier")), "Id")


  if(length(tmp_cands) > 0) {

    tmp_candidates <- rbind(tmp_candidates,
                            data.frame(DivisionID = DivisionID,
                                       CandidateID = tmp_cands,
                                       Type = "Candidate",
                                       stringsAsFactors = FALSE))

  }

  if(length(tmp_ghosts) > 0) {

    tmp_candidates <- rbind(tmp_candidates,
                            data.frame(DivisionID = DivisionID,
                                       CandidateID = tmp_ghosts,
                                       Type = "Ghost",
                                       stringsAsFactors = FALSE))

  }

  if(length(tmp_tcp) > 0) {

    tmp_candidates <- rbind(tmp_candidates,
                            data.frame(DivisionID = DivisionID,
                                       CandidateID = tmp_tcp,
                                       Type = "TCP",
                                       stringsAsFactors = FALSE))

  }

  tmp_candidates <- rbind(tmp_candidates,
                          data.frame(DivisionID = DivisionID,
                                     CandidateID = c(1, 2),
                                     Type = "TPP",
                                     stringsAsFactors = FALSE),
                          data.frame(DivisionID = DivisionID,
                                     CandidateID = NA,
                                     Type = c("Formal", "Informal", "Total"),
                                     stringsAsFactors = FALSE))
  return(tmp_candidates)

}

get_mediafeed_votes_div <- function(df, xml) {

  tmp_div <- df[1]
  tmp_cand <- df[2]
  tmp_type <- df[3]

  if(tmp_type %in% c("Candidate", "Ghost")) {

    tmp_root <- paste0("//d1:House/d1:Contests/*/eml:ContestIdentifier[@Id=",
                       tmp_div,
                       "]/../d1:FirstPreferences/*/eml:CandidateIdentifier[@Id=",
                       tmp_cand,
                       "]/../")

  } else if(tmp_type == "TCP") {

    tmp_root <- paste0("//d1:House/d1:Contests/*/eml:ContestIdentifier[@Id=",
                       tmp_div,
                       "]/../d1:TwoCandidatePreferred/*/eml:CandidateIdentifier[@Id=",
                       tmp_cand,
                       "]/../")

  } else if(tmp_type == "TPP") {

    tmp_root <- paste0("//d1:House/d1:Contests/*/eml:ContestIdentifier[@Id=",
                       tmp_div,
                       "]/../d1:TwoPartyPreferred/d1:Coalition/d1:CoalitionIdentifier[@Id=",
                       tmp_cand,
                       "]/../")

  } else if(tmp_type %in% c("Formal", "Informal", "Total")) {

    tmp_root <- paste0("//d1:House/d1:Contests/*/eml:ContestIdentifier[@Id=",
                       tmp_div,
                       "]/../d1:FirstPreferences/d1:",
                       tmp_type,
                       "/")

  }

  tmp_total <- xml_text(xml_find_all(xml, paste0(tmp_root, "d1:Votes")))
  tmp_total_hist <- xml_attr(xml_find_all(xml, paste0(tmp_root, "d1:Votes")), "Historic")
  tmp_total_swing <- xml_attr(xml_find_all(xml, paste0(tmp_root, "d1:Votes")), "Swing")
  tmp_type_type <- xml_attr(xml_find_all(xml, paste0(tmp_root, "d1:VotesByType/d1:Votes")), "Type")
  tmp_type_hist <- xml_attr(xml_find_all(xml, paste0(tmp_root, "d1:VotesByType/d1:Votes")), "Historic")
  tmp_type_swing <- xml_attr(xml_find_all(xml, paste0(tmp_root, "d1:VotesByType/d1:Votes")), "Swing")
  tmp_type_votes <- xml_text(xml_find_all(xml, paste0(tmp_root, "d1:VotesByType/d1:Votes")))

  tmp_votes_df <- rbind(data.frame(Votes = c(tmp_total, tmp_type_votes),
                                   Which = c("Votes"),
                                   VoteType = c("DivTotal", tmp_type_type),
                                   stringsAsFactors = FALSE),
                        data.frame(Votes = c(tmp_total_hist, tmp_type_hist),
                                   Which = c("Historic"),
                                   VoteType = c("DivTotal", tmp_type_type),
                                   stringsAsFactors = FALSE),
                        data.frame(Votes = c(tmp_total_swing, tmp_type_swing),
                                   Which = c("Swing"),
                                   VoteType = c("DivTotal", tmp_type_type),
                                   stringsAsFactors = FALSE))
  tmp_votes_df$DivisionID <- tmp_div
  tmp_votes_df$CandidateID <- tmp_cand
  tmp_votes_df$CandidateType <- tmp_type

  # tmp_votes_df$Votes <- as.integer(tmp_votes_df$Votes)

  return(tmp_votes_df[c("DivisionID", "CandidateID", "CandidateType",
                        "VoteType", "Which", "Votes")])

}


get_mediafeed_divisionids <- function(xml) {

  xml_attr(xml_find_all(xml, "//d1:House/d1:Contests/*/eml:ContestIdentifier"), "Id")

}

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

