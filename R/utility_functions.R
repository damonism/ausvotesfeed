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
#' @param Download (\code{LOGICAL}) If \code{TRUE} (default) download the file
#'   and return the path on the local disk. If \code{FALSE}, don't download the
#'   file but return the URL where it located.
#'
#' @return A file path of the downloaded file or the URL of the file
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
download_mediafeed_file <- function(EventIdentifier, Filetype, Detail = "Detailed", Dest = tempdir(), Archive = TRUE, Download = TRUE) {

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

  dir_con <- curl(path, "r")
  dir <- readLines(dir_con)
  close(dir_con)
  filename <- rev(strsplit(tail(dir, 1), " +")[[1]])[1]
  fileurl <- paste0(path, filename)

  if(Download) {

    if(!dir.exists(Dest)) {
      dir.create(Dest)
    }

    down_con <- curl_download(fileurl, paste0(Dest, "/", filename), quiet = FALSE, mode = "wb")
    # close(down_con)

    return(paste0(Dest, "/", filename))

  } else{

    return(fileurl)

  }
}

#' Download media feed files via the plumber API
#'
#' The AEC media feed is hosted on an anonymous FTP site, which may be blocked
#' by some firewalls. This function works in conjunction with a \pkg{plumber}
#' script, which is included in the \code{plumber} directory in this package.
#'
#' Simply run the included \code{run.R} file in the \pkg{plumber} directory on a
#' server that can access the media feed FTP server and use the resulting server
#' address and port with this function to download the latest zipfile of the
#' appropriate type from the media feed.
#'
#' The \code{api.R} file in the \pkg{plumber} directory runs
#' \code{\link{download_mediafeed_file}} with the correct parameters and sends
#' the resulting file, which is then downloaded by this function.
#'
#' Note that the function does not give any feedback when it is downloading the
#' file (which might take a while as some of them are large), but does do a bit
#' of error checking when the download is done.
#'
#' @param Server The URL of the server, including protocol and port, with no
#'   trailing slash
#' @param EventIdentifier AEC event identifier or election year
#' @param Filetype One of \code{Light}, \code{Preload} or \code{Verbose}
#' @param Archive If \code{TRUE} (default), use the AEC's media feed archive
#'   site, or \code{FALSE} for the live election results
#'
#' @return A string with the full path of the downloaded file.
#' @export
#'
#' @examples
#' \dontrun{
#' download_mediafeed_api("http://127.0.0.1:3379", 2022, "Preload", Archive = FALSE)}
#' @importFrom curl curl_fetch_disk parse_headers new_handle
download_mediafeed_api <- function(Server, EventIdentifier, Filetype, Archive = TRUE) {

  if(Filetype == "Preload") {
    tmp_endpoint <- "/preload"
  } else if (Filetype == "Verbose") {
    tmp_endpoint <- "/verbose"
  } else if (Filetype == "Light") {
    tmp_endpoint <- "/light"
  } else {
    stop("Filetype must be one of 'Preload', 'Verbose' or 'Light")
  }

  tmp_id <- vtr_identifier(EventIdentifier, as = "event")
  tmp_url <- paste0(Server, tmp_endpoint, "?election=", EventIdentifier, "&archive=", Archive)

  tmp_zip_h <- new_handle()
  tmp_tempfile <- tempfile()
  tmp_trans <- curl_fetch_disk(tmp_url, tmp_tempfile, handle = tmp_zip_h)
  tmp_zip_headers <- parse_headers(tmp_trans$headers)
  tmp_zip_status <- strsplit(tmp_zip_headers[grep("^HTTP", tmp_zip_headers)], " ", fixed = TRUE)[[1]]
  tmp_zip_size <- as.integer(gsub("Content-Length: ", "",
                                  tmp_zip_headers[grep("Content-Length", tmp_zip_headers, fixed = TRUE)],
                                  fixed = TRUE))

  if(tmp_zip_status[2] != "200") {

    stop("Something went wrong: ", paste(tmp_zip_status, collapse = " "))

  }

  tmp_zip_filename <- strsplit(tmp_zip_headers[grep("Content-Disposition: attachment", tmp_zip_headers, fixed = TRUE)], "\"")[[1]][2]
  tmp_newfile <- paste0(tempdir(), "/", tmp_zip_filename)
  if(!file.copy(tmp_tempfile, tmp_newfile)) {
    stop("Failed to copy from ", tmp_tempfile, " to ", tmp_newfile)
  }

  if(file.info(tmp_newfile)$size != tmp_zip_size) {
    stop("File ", tmp_newfile, " is not the same size as the download file (download: ", tmp_zip_size, ", new file: ", file.info(tmp_newfile)$size, ").")
  }

  return(tmp_newfile)

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
#' read_mediafeed_xml("/tmp/aec-mediafeed-Detailed-Preload-27966-20220503123540.zip",
#'                    "results")
#'
#' # Read a file that has been downloaded with read_mediafeed_xml()
#' read_mediafeed_xml(download_mediafeed_file(2022, "Preload", "Detailed", "/tmp"))}
#'
#' @seealso \code{\link{download_mediafeed_file}} for downloading the correct
#'   file from the AEC's FTP site.
#'
#' @importFrom xml2 read_xml xml_attrs xml_find_first
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
    if(length(tmp_old_files) > 0) {
      message("Found ", length(tmp_old_files), " old XML files. Deleting... " , appendLF = FALSE)
      unlink(tmp_old_files)
      message("done.")
    }

    unzip(path, overwrite = TRUE, junkpaths = TRUE, exdir = tmp_unzipdir)
    message("Unzipping to ", tmp_unzipdir)
    if(!is.na(filename)) {
      tmp_xml_file <- list.files(tmp_unzipdir, pattern = paste0(filename, ".*\\.xml"), full.names = TRUE)
    } else {
      tmp_xml_file <- list.files(tmp_unzipdir, pattern = paste0("aec-mediafeed-results-.*", vtr_id, "\\.xml"), full.names = TRUE)
    }
    message("Reading XML file: ", tmp_xml_file)

    tmp_xml <- xml2::read_xml(tmp_xml_file)
    if(!filename %in% c("candidates", "event")) {
    message("Created: ",
            as.POSIXct(xml_attrs(xml_find_first(tmp_xml, "/d1:MediaFeed"))[["Created"]],
                       format = "%FT%T"))
    }
    return(tmp_xml)
  }
}

#' Print media feed metadata
#'
#' Display metadata from the media feed XML file.
#'
#' Display some metadata from the media feed XML file, including when it was
#' updated, its verbosity and event identifier.
#'
#' This should be relatively quick because it only accesses information at the
#' very top of the file (although there is inevitably some delay in parsing even
#' the first bit of such a large file).
#'
#' The time stamp returned in \code{Created} can be converted with
#' \code{"\%FT\%T"} as a format string.
#'
#' @param xml A pointer to an XML media feed object.
#' @param short \code{TRUE} to display just the most important items (default)
#'   or \code{FALSE} to display everything.
#'
#' @return A named vector.
#' @export
#'
#' @examples
#' \dontrun{
#' results_xml <- read_mediafeed_xml(download_mediafeed_file(2022, "Verbose", Archive = FALSE))
#' get_mediafeed_metadata(results_xml)}
#'
#' @importFrom xml2 xml_attrs xml_find_first
get_mediafeed_metadata <- function(xml, short = TRUE) {

  # tmp_created <- xml_attrs(xml_find_first(xml, "/d1:MediaFeed"))[["Created"]]
  #
  # return(tmp_created)

  tmp_metadata <- c(xml_attrs(xml_find_first(xml, "/d1:MediaFeed")),
                    xml_attrs(xml_find_first(xml, "d1:Results")),
                    EventIdentifier = xml_attrs(xml_find_first(xml, "d1:Results/eml:EventIdentifier"))[[1]])
  # tmp_metadata["Created"] <- as.POSIXct(tmp_metadata["Created"], format = "%FT%T")

  if(short) {
    return(tmp_metadata[c("Created", "Phase", "Verbosity", "Granularity", "EventIdentifier")])
  }else {
    return(tmp_metadata)
  }
}

#' Fill down
#'
#' Internal convenience fill down function to avoid including all of \code{tidyr}.
#'
#' @param x A vector or \code{data.frame} column.
#'
#' @return A vector or \code{data.frame} column.
#'
#' @keywords internal
#'
#' @source \url{https://stackoverflow.com/questions/9514504/add-missing-value-in-column-with-value-from-row-above}
Fill <- function(x)
{
  Log <- !is.na(x)
  y <- x[Log]
  y[cumsum(Log)]
}
