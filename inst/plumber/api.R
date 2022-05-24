library(plumber)
library(curl)
library(ausvotesfeed)

#* @get /preload
#* @serializer contentType list(type="application/octet-stream")
fetch_mediafeed_preload <- function(election, archive) {
  tmp_url <- ausvotesfeed::download_mediafeed_file(EventIdentifier = election,
                                        Filetype = "Preload",
                                        Detail = "Detailed",
                                        Dest = tempdir(),
                                        Archive = archive,
                                        Download = FALSE)
  tmp_filename <- rev(strsplit(tmp_url, "/")[[1]])[1]

  tmp_zipfile <- curl::curl_fetch_memory(tmp_url)

  plumber::as_attachment(tmp_zipfile$content, tmp_filename)
}

#* @get /verbose
#* @serializer contentType list(type="application/octet-stream")
fetch_mediafeed_preload <- function(election, archive) {
  tmp_url <- ausvotesfeed::download_mediafeed_file(EventIdentifier = election,
                                                   Filetype = "Verbose",
                                                   Detail = "Detailed",
                                                   Dest = tempdir(),
                                                   Archive = archive,
                                                   Download = FALSE)
  tmp_filename <- rev(strsplit(tmp_url, "/")[[1]])[1]

  tmp_zipfile <- curl::curl_fetch_memory(tmp_url)

  plumber::as_attachment(tmp_zipfile$content, tmp_filename)
}

#* @get /light
#* @serializer contentType list(type="application/octet-stream")
fetch_mediafeed_preload <- function(election, archive) {
  tmp_url <- ausvotesfeed::download_mediafeed_file(EventIdentifier = election,
                                                   Filetype = "Light",
                                                   Detail = "Detailed",
                                                   Dest = tempdir(),
                                                   Archive = archive,
                                                   Download = FALSE)
  tmp_filename <- rev(strsplit(tmp_url, "/")[[1]])[1]

  tmp_zipfile <- curl::curl_fetch_memory(tmp_url)

  plumber::as_attachment(tmp_zipfile$content, tmp_filename)
}
