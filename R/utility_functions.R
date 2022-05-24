#' Retrieve the xml files from the media feed
#'
#' These functions are used to retrieve the files from the AEC's FTP server
#' and make the contained XML available to be used with the package's
#' other functions.
#'
#' @name utility_functions
NULL

#' Download file from AEC media feed FTP site
#'
#' Downloads a specific files from the AEC's media feed FTP site and returns the
#' filename and path of the file.
#'
#' @param EventIdentifier AEC event identifier or election year
#' @param Filetype One of \code{Light}, \code{Preload}, \code{PollingDistricts}
#'   or \code{Verbose}
#' @param Detail One of either \code{Detailed} (default) or \code{Standard}
#' @param Dest Destination path for the downloaded file (will be created if it
#'   does not exist) and defaults to \code{tempdir()}
#' @param Archive If \code{TRUE} (default), use the AEC's media feed archive
#'   site, or \code{FALSE} for the live election results
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
#' @importFrom utils tail
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
#' Read the media feed XML file and return an \code{xml2} pointer, unzipping the
#' file if necessary.
#'
#' This is either a very thin wrapper around \code{xml2::read_xml()} or an easy
#' way to access a media feed XML file that is contained in a zip file, such as
#' one downloaded by \code{\link{download_mediafeed_file}}.
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
#' @importFrom utils unzip tail
read_mediafeed_xml <- function(path, filename = NA) {

  filenames <- c("results", "pollingdistricts", "event", "candidates")
  if(!is.na(filename)) {
    if(!filename %in% filenames) {
      stop(filename, " is not a valid filename.")
    }
  }

  filetype <- rev(strsplit(path, ".", fixed = TRUE)[[1]])[1]
  message("File type is: ", filetype)

  vtr_id <- grep("^[0-9]{5}$", strsplit(rev(strsplit(path, "/", fixed = TRUE)[[1]])[1], "-", fixed = TRUE)[[1]], value = TRUE)
  tmp_filename <- tail(strsplit(path, "/")[[1]], 1)
  tmp_is_preload <- ifelse(grepl("Preload", tmp_filename, fixed = TRUE), TRUE, FALSE)

  if(filetype == "xml") {
    return(xml2::read_xml(path))
  }

  if(filetype == "zip") {

    if(tmp_is_preload) {
      if(is.na(filename)) {
        stop(tmp_filename, " appears to be a preload zip file; filename argument is required.")
      }
    }

    tmp_unzipdir <- tempdir()

    # Remove any old XML files
    tmp_old_files <- list.files(tmp_unzipdir, pattern = ".*\\.xml", full.names = TRUE)
    message("Found ", length(tmp_old_files), " old XML files. Deleting... " , appendLF = FALSE)
    unlink(tmp_old_files)
    message("done.")

    unzip(path, overwrite = TRUE, junkpaths = TRUE, exdir = tmp_unzipdir)
    message("Unzipping to ", tmp_unzipdir)
    if(!is.na(filename)) {
      tmp_xml_file <- list.files(tmp_unzipdir, pattern = paste0(filename, ".*\\.xml"), full.names = TRUE)
    } else {
      tmp_xml_file <- list.files(tmp_unzipdir, pattern = paste0("aec-mediafeed-results-.*", vtr_id, "\\.xml"), full.names = TRUE)
    }
    message("Reading XML file: ", tmp_xml_file)
    return(xml2::read_xml(tmp_xml_file))
  }
}
