library(ausvotesfeed)
library(dplyr)

preload_xml <- read_mediafeed_xml(download_mediafeed_file(2022, Filetype = "Preload", Archive = TRUE),
                                  filename = "results")

mf_cand <- get_mediafeed_preload_candidates(preload_xml)
mf_div <- get_mediafeed_preload_divs(preload_xml)
mf_pps <- get_mediafeed_preload_pps(preload_xml)
mf_pps_cand <- get_mediafeed_preload_pps_cand(preload_xml)
mf_votes <- get_mediafeed_preload_votes_type(preload_xml)

cand_xml <- read_mediafeed_xml(download_mediafeed_file(2022, Filetype = "Preload", Archive = TRUE),
                               filename = "candidates")

mf_cand <- mf_cand %>%
  left_join(get_mediafeed_preload_gender(cand_xml, "House") %>%
              select(CandidateID, Gender),
            by = "CandidateID")

tmp_pps <- download_file_vtr("GeneralPollingPlacesDownload", 27966, "https://tallyroom.aec.gov.au/Downloads/")

mf_pps <- mf_pps %>%
  left_join(tmp_pps %>%
              select(-PollingPlaceNm) %>%
              rename(StateAb = State),
            by = c("DivisionID", "PollingPlaceID"))

save(list = ls(pattern = "mf_"), file = "preload_2022.rds")
