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

