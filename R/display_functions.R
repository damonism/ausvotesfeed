
mediafeed_display_tcp <- function(xml, preload_candidates) {
  tmp_div_tcp <- get_mediafeed_votes_div(xml, "tcp")
  tmp_div_tcp$.order <- 1:nrow(tmp_div_tcp)

  tmp_cand <- preload_candidates
  tmp_cand[c("FP.Votes", "FP.Percentage", "FP.Swing", "FP.Historic", "FP.MatchedHistoric")] <- NULL

  tmp_margin_df <- data.frame(CandidateId = tmp_div_tcp$CandidateId[tmp_div_tcp$TCP.Percentage > 50],
                              TCP.Margin = tmp_div_tcp$TCP.Votes[tmp_div_tcp$TCP.Percentage > 50] - tmp_div_tcp$TCP.Votes[tmp_div_tcp$TCP.Percentage < 50])

  tmp_div_tcp <- merge(tmp_div_tcp, tmp_margin_df, by = c("CandidateId"), all.x = TRUE)

  tmp_div_tcp <- merge(tmp_div_tcp, tmp_cand, by = c("DivisionId", "CandidateId", "CandidateType"), all.x = TRUE)

  tmp_div_tcp$PartyNm <- ifelse(tmp_div_tcp$IsIndependent == TRUE, "Independent", tmp_div_tcp$PartyNm)
  tmp_div_tcp$Status <- ifelse((tmp_div_tcp$ElectedHistoric == TRUE | tmp_div_tcp$Incumbent == TRUE) & tmp_div_tcp$Elected == TRUE, "Re-elected",
                               ifelse(tmp_div_tcp$ElectedHistoric == FALSE & tmp_div_tcp$Elected == TRUE, "Elected",
                                      ifelse(tmp_div_tcp$ElectedHistoric == TRUE | tmp_div_tcp$Incumbent == TRUE, "Previous Member",
                                             ifelse(tmp_div_tcp$IncumbentNotional == TRUE, "Notional Incumbent", ""))))

  tmp_div_tcp <- tmp_div_tcp[match(1:nrow(tmp_div_tcp), tmp_div_tcp$.order),]
  tmp_div_tcp$.order <- NULL
  rownames(tmp_div_tcp) <- NULL

  return(tmp_div_tcp)
}

mediafeed_display_fp <- function(xml, preload_candidates) {
  tmp_div_fp <- get_mediafeed_votes_div(xml, "fp")
  tmp_div_total <- get_mediafeed_votes_div(xml, "total")

  tmp_cand <- preload_candidates
  tmp_cand[c("FP.Votes", "FP.Percentage", "FP.Swing", "FP.Historic", "FP.MatchedHistoric")] <- NULL

  # This creates an order variable to allow the totals for each division to be below the candidates for that division.
  tmp_div_fp$.order <- unlist(lapply(unique(tmp_div_fp$DivisionId),
                                     function(x) x * 1000 + 1:nrow(tmp_div_fp[tmp_div_fp$DivisionId == x,])))
  tmp_div_total$.order <- unlist(lapply(unique(tmp_div_total$DivisionId), function(x) x * 1000 + 997:999))

  tmp_div_fp <- merge(tmp_div_fp, tmp_cand, by = c("DivisionId", "CandidateId", "CandidateType"), all.x = TRUE)

  tmp_div_fp$PartyNm <- ifelse(tmp_div_fp$IsIndependent == TRUE, "Independent", tmp_div_fp$PartyNm)
  tmp_div_fp$Status <- ifelse((tmp_div_fp$ElectedHistoric == TRUE | tmp_div_fp$Incumbent == TRUE) & tmp_div_fp$Elected == TRUE, "Re-elected",
                              ifelse(tmp_div_fp$ElectedHistoric == FALSE & tmp_div_fp$Elected == TRUE, "Elected",
                                     ifelse(tmp_div_fp$ElectedHistoric == TRUE | tmp_div_fp$Incumbent == TRUE, "Previous Member",
                                            ifelse(tmp_div_fp$IncumbentNotional == TRUE, "Notional Incumbent", ""))))

  tmp_div_total$CandidateNm <- tmp_div_total$CandidateType
  tmp_div_total[setdiff(colnames(tmp_div_fp), colnames(tmp_div_total))] <- NA

  tmp_div <- bind_rows(tmp_div_fp, tmp_div_total)
  tmp_div <- tmp_div[match(sort(tmp_div$.order), tmp_div$.order),]
  rownames(tmp_div) <- NULL
  tmp_div[c("StateAb", "DivisionShortCode", "DivisionNm")] <- lapply(tmp_div[c("StateAb", "DivisionShortCode", "DivisionNm")], Fill)

  return(tmp_div)
}
