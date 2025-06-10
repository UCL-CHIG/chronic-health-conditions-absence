
chc_cols <- names(cohort_eligible)[grepl("chc_", names(cohort_eligible))]

# removing minor CHCs
chc_cols <- chc_cols[!grepl("kidney|cleft|cystic|ibs|chd|cell|spina",
                            chc_cols)]

# specify indicator for all children (useful for PARFs later)
for (i in 6:10) {
  new_col_all <- paste0("chc_all_children_", i)
  cohort_eligible[, (new_col_all) := T]
  
  new_col_exp <- paste0("chc_unexposed_", i)
  chc_any_col <- paste0("chc_any_", i)
  cohort_eligible[, (new_col_exp) := !(get(chc_any_col))]
  
}

rm(i, new_col_all)


chc_cols <- c(chc_cols,
              names(cohort_eligible)[grepl("chc_unexposed", names(cohort_eligible))],
              names(cohort_eligible)[grepl("chc_all_children", names(cohort_eligible))])

chc_cols <- c(chc_cols[grepl("chc_any", chc_cols)],
              chc_cols[grepl("chc_hardelid", chc_cols)],
              chc_cols[grepl("chc_asthma", chc_cols)],
              chc_cols[grepl("chc_cerebral", chc_cols)],
              chc_cols[grepl("chc_diabetes", chc_cols)],
              chc_cols[grepl("chc_epilepsy", chc_cols)],
              chc_cols[grepl("chc_neurodis", chc_cols)],
              chc_cols[grepl("chc_mh", chc_cols)],
              chc_cols[grepl("chc_srp_internalising", chc_cols)],
              chc_cols[grepl("chc_srp_externalising", chc_cols)],
              chc_cols[grepl("chc_srp_psych", chc_cols)],
              chc_cols[grepl("chc_ara", chc_cols)],
              chc_cols[grepl("chc_unexposed", chc_cols)],
              chc_cols[grepl("chc_all_children", chc_cols)])

chc_groups <- unique(gsub("_[0-9].*", "", chc_cols))
chc_groups <- gsub("chc_", "", chc_groups)

