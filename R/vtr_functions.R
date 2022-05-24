#' Read VTR CVS file
#'
#' Read results files in comma separated variable format from the AEC Virtual
#' Tally Room.
#'
#' The AEC's Virtual Tally Room files come in both CSV and tab delineated
#' format, however they have a few quirks that means that sending them straight
#' to \link{read.csv} will not work.
#'
#' In particular, they have a single line of text before the CSV file starts
#' that contains a bit of meta-data about the file. This is particularly useful
#' when the files are still being updated after an election.
#'
#' This function reads the CSV file and appends some of the metadata as
#' attributes to the resulting \code{data.frame}.
#'
#' Because this function use both \code{\link{read.csv}} and
#' \code{\link{read.table}} internally, it can also be used on remote files.
#' Note however that it will have to download them twice, which may be
#' inefficient.
#'
#' @param filename The file name of the CSV file
#' @param ... Passed to \link{read.csv}
#'
#' @return A \code{data.frame} of the file with two attributes: \code{updated},
#'   which is the time the file was updated in \code{POSIXct} format, and
#'   \code{aec.metadata}, which is the full metadata, unformatted.
#'
#' @export
#'
#' @examples
#' url <- "https://results.aec.gov.au/24310/Website/Downloads/GeneralPartyDetailsDownload-24310.csv"
#' read_cvs_vtr(url)
#'
#' \dontrun{
#' read_cvs_vtr("HouseTcpByCandidateByVoteTypeDownload-27966.csv")
#' }
#'
#' @importFrom utils read.csv read.table
read_cvs_vtr <- function(filename, ...) {

  tmp_csv_table <- read.csv(filename, sep = ',', header = TRUE, skip = 1, stringsAsFactors = FALSE, ...)
  tmp_csv_tag <- as.character(read.table(filename, nrows = 1, as.is = TRUE, sep = "|"))

  tmp_date_match <- regexpr("([0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2})", tmp_csv_tag)
  tmp_date_text <- substring(tmp_csv_tag,
                             tmp_date_match,
                             tmp_date_match + attr(tmp_date_match, "match.length") - 1)
  tmp_date <- as.POSIXct(tmp_date_text, format = "%FT%X")

  attr(tmp_csv_table, "updated") <- tmp_date
  attr(tmp_csv_table, "aec.metadata") <- tmp_csv_tag

  return(tmp_csv_table)

}

#' Download files from AEC Virtual Tally Room
#'
#' Download results files from AEC Virtual Tally Room (VTR) and return a
#' \code{data.frame} of the data.
#'
#' The results files on the VTR store the CSV files as data but they are
#' packages in a number of ways, including as multiple files in a .zip archive,
#' or as seaprate files for each state and territory (or both). This function
#' uses the predictable naming structure of the files to get the correct files
#' and return them as a \code{data.frame} of national results.
#'
#' The function uses \code{\link{read_cvs_vtr}} internally to read the files,
#' which is a thin wrapper around \link{read.csv}, but also adds some metadata
#' to the files as attributes.
#'
#' @param file The name of the file to retrieve
#' @param election The AEC's event identifier for the election or the election
#'   year.
#' @param basedir (Optional) The URL of the Vitrual Tally Room Page - only
#'   necessary for results of current elections that have not yet been archived
#'   to results.aec.gov.au
#' @param verbose (Optional) Be more verbose
#' @param cached (Optional) Force using cached files, rather than re-downloading
#'   files from the VTR
#'
#' @return A \code{data.frame} with the CSV data as parsed by \code{read.csv},
#'   with state/territory files combined where necessary.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' download_file_vtr("HouseCandidatesDownload",
#'                   24310,
#'                   "https://tallyroom.aec.gov.au/Downloads/")
#' }
#'
#' @seealso \code{\link{read_cvs_vtr}} for the function that actually reads the
#'   CSV files that have been retrieved.
#'
#' @importFrom utils unzip download.file tail
#' @importFrom rappdirs user_data_dir
download_file_vtr <- function(file, election, basedir = NA, verbose = FALSE, cached = TRUE){

  event_id_df <- vtr_ids()

  election <- as.integer(election)

  if(election %in% event_id_df$EventId) {

    event_id <- election

  } else if (election %in% event_id_df$ElectionYear) {

    event_id <- event_id_df$EventId[event_id_df$ElectionYear == election]

  } else if (grepl("^[0-9]{5}$", election)) {

    message("Assuming ", election, " is a valid event identifier.")

  } else {

    stop(election, " is not a valid event identifier or election year.")

  }

  tmp_cache_dir <- rappdirs::user_data_dir(appname = "ausvotes")
  if(!dir.exists(tmp_cache_dir)) {
    dir.create(tmp_cache_dir, recursive = TRUE)
  }

  tmp_use_cache <- ifelse(!is.na(basedir) | cached == FALSE, FALSE, TRUE)

  if(verbose) {
    if(tmp_use_cache == FALSE) {
      message("Not using cached files.")
    }
  }

  # Allow a whole URL to serve as the file argument - an undocumented convenience feature
  tmp_parse_file <- parse_vtr_url(file)
  if(length(tmp_parse_file) > 1) {
    file <- tmp_parse_file["filename"]
    basedir <- tmp_parse_file["basedir"]
  }

  # The VTR is a bit inconsistent about where its files live.
  if(is.na(basedir)) {

    if(event_id == 12246) {

      basedir <- paste0("https://results.aec.gov.au/", event_id, "/results/Downloads/")

    } else {

      basedir <- paste0("https://results.aec.gov.au/", event_id, "/Website/Downloads/")

    }

  }

  # Files that need to be assembled by state
  states_list <- c('NSW', 'VIC', 'QLD', 'WA', 'SA', 'TAS', 'ACT', 'NT')
  states_files <- c('HouseStateFirstPrefsByPollingPlaceDownload')

  # Files that are zipped on the VTR
  zipfiles_files <- c('HouseTcpFlowByPPDownload', 'HouseDopByPPDownload',
                      'SenateStateFirstPrefsByPollingPlaceDownload')

  if(!grepl("/$", basedir)) {
    basedir <- paste0(basedir, "/")
  }

  # message("Downloading ", file, appendLF = FALSE)

  isQuiet <- ifelse(verbose, FALSE, TRUE)

  if(file %in% states_files){

    tmp_vtr_table <- data.frame()
    tmp_url_list <- paste0(basedir, file, '-', event_id, '-', states_list, '.csv')

    for(tmp_file_url in tmp_url_list) {

      tmp_file_local <- paste0(tmp_cache_dir, "/", tail(strsplit(tmp_file_url, "/")[[1]] , n = 1))

      if(!file.exists(tmp_file_local) | tmp_use_cache == FALSE) {

        if(verbose) {message("Downloading ", tmp_file_url, appendLF = TRUE)}
        download.file(tmp_file_url, tmp_file_local, quiet = isQuiet)

      }

      if(verbose) {message("Reading ", tmp_file_local, appendLF = TRUE)}

      tmp_vtr_table_state <- read_cvs_vtr(tmp_file_local)
      tmp_vtr_table <- rbind(tmp_vtr_table, tmp_vtr_table_state)

    }

  } else if (file %in% zipfiles_files) {

    tmp_vtr_table <- data.frame()

    # Zip files have a different basedir
    if(event_id == 12246) {

      tmp_url_list <- paste0("https://results.aec.gov.au/",
                             event_id,
                             "/results/External/",
                             file,
                             "-",
                             event_id,
                             "-",
                             states_list,
                             ".zip")

    } else {

      tmp_url_list <- paste0("https://results.aec.gov.au/",
                             event_id,
                             "/Website/External/",
                             file,
                             "-",
                             event_id,
                             "-",
                             states_list,
                             ".zip")

    }

    for(tmp_file_url in tmp_url_list) {

      base_tempdir <- tempdir()
      # zip_tempfile <- tempfile(fileext = ".zip")
      csv_tempdir <- paste0(base_tempdir, "/vtr_download_csv")

      # If the function has exited abnormally the directory might still be there
      if(dir.exists(csv_tempdir)) {
        if(verbose) { message("Old download directory found (", csv_tempdir, "). Deleting. ", appendLF = FALSE) }
        unlink(csv_tempdir, recursive = TRUE)
        if(verbose) { message("Done.") }
      }

      # # If the function has exited abnormally the file might still be there
      # if(file.exists(zip_tempfile)) {
      #   if(verbose) { message("Old download directory found (", csv_tempdir, "). Deleting. ", appendLF = FALSE) }
      #   unlink(zip_tempfile)
      #   if(verbose) { message("Done.") }
      # }

      dir.create(csv_tempdir)

      tmp_file_local <- paste0(tmp_cache_dir, "/", tail(strsplit(tmp_file_url, "/")[[1]] , n = 1))

      if(!file.exists(tmp_file_local) | tmp_use_cache == FALSE) {

        if(verbose) {message("Downloading ", tmp_file_url, appendLF = TRUE)}
        download.file(tmp_file_url, tmp_file_local, quiet = isQuiet)

      } else {

        if(verbose) {message("Found ", tmp_file_local, appendLF = TRUE)}

      }

      # if(verbose) { message("Downloading ", tmp_file_url, appendLF = TRUE) }
      # download_quiet <- ifelse(verbose, FALSE, TRUE)
      # download.file(tmp_file_url, zip_tempfile, quiet = download_quiet)
      # if(verbose) { message("Done.") }
      if(verbose) { message("Unzipping... ", appendLF = FALSE) }
      unzip(tmp_file_local, junkpaths = TRUE, exdir = csv_tempdir)
      csv_files <- list.files(path = csv_tempdir, pattern = "\\.csv", full.names = TRUE)
      if(verbose) { message("Extracted ", length(csv_files), " CSV files.") }

      if(verbose) { message("Reading files", appendLF = FALSE) }
      for(each_file in csv_files) {

        if(verbose) { message(".", appendLF = FALSE) }
        tmp_vtr_table <- rbind(tmp_vtr_table,
                               read_cvs_vtr(each_file,
                                        skipNul = TRUE))  # Some of the Senate CSVs have nulls.


      }

      if(verbose) { message(" Done.") }

      # unlink(zip_tempfile)
      unlink(csv_tempdir, recursive = TRUE)

    }

  } else {

    tmp_file_url <- paste(basedir, file, '-', event_id, '.csv', sep = '')

    tmp_file_local <- paste0(tmp_cache_dir, "/", tail(strsplit(tmp_file_url, "/")[[1]] , n = 1))

    if(!file.exists(tmp_file_local) | tmp_use_cache == FALSE) {

      if(verbose) {message("Downloading ", tmp_file_url, appendLF = TRUE)}
      download.file(tmp_file_url, tmp_file_local, quiet = isQuiet)

    }

    if(verbose) {message("Reading ", tmp_file_local, appendLF = TRUE)}

    tmp_vtr_table <- read_cvs_vtr(tmp_file_local)

    if(verbose) {message("Done.", tmp_file_local)}

  }

  # message(" Done.")

  if(nrow(tmp_vtr_table) < 1 | ncol(tmp_vtr_table) < 1) {
    stop("Nothing downloaded.")
  }

  return(tmp_vtr_table)

}

parse_vtr_url <- function(filename) {
  # TODO: add election identifier

  if(grepl("aec.gov.au", filename, fixed = TRUE)) {
    tmp_url_components <- strsplit(filename, "/", fixed = TRUE)[[1]]
    tmp_host <- tmp_url_components[grep("aec.gov.au", tmp_url_components, fixed = TRUE)]
    tmp_protocol <- tmp_url_components[grep("http", tmp_url_components, fixed = TRUE)]
    # This assumes that the path only has one component. Not sure if that is true!
    tmp_path <- tmp_url_components[grep("aec.gov.au", tmp_url_components, fixed = TRUE) + 1]
    tmp_basedir <- paste0(tmp_protocol, "//", tmp_host, "/", tmp_path, "/")
    tmp_csvfile <- rev(tmp_url_components)[1]
    tmp_filename <- gsub("-[0-9]{5}.*$", "", tmp_csvfile)

    return(c("host" = tmp_host,
             "protocol" = tmp_protocol,
             "path" = tmp_path,
             "csvfile" = tmp_csvfile,
             "basedir" = tmp_basedir,
             "filename" = tmp_filename))
  } else {
    return(NA)
  }
}
