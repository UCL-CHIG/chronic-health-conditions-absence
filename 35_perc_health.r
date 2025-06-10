
# abs_cols ----------------------------------------------------------------

abs_cols <- names(cohort_eligible)[grepl("perc_health", names(cohort_eligible))]

# # overall -----------------------------------------------------------------

abs_data <- data.table(
  exposure = chc_cols,
  exposure_collapse = rep(chc_groups, each = 5)
)

for (i in 1:nrow(abs_data)) {

  cur_chc <- abs_data[i, exposure]
  print(cur_chc)

  for (abs_col in abs_cols) {

    n <- sum(!is.na(cohort_eligible[get(cur_chc) == T, get(abs_col)]))
    med <- median(cohort_eligible[get(cur_chc) == T, get(abs_col)], na.rm = T)
    q25 <- quantile(cohort_eligible[get(cur_chc) == T, get(abs_col)], probs = 0.25, na.rm = T)
    q75 <- quantile(cohort_eligible[get(cur_chc) == T, get(abs_col)], probs = 0.75, na.rm = T)

    n_col <- paste0(abs_col, "_n")
    med_col <- paste0(abs_col, "_med")
    q25_col <- paste0(abs_col, "_q25")
    q75_col <- paste0(abs_col, "_q75")
    text_col <- paste0(abs_col, "_med_iqr")

    abs_data[i, ((n_col)) := n]
    abs_data[i, ((text_col)) := paste0(round(med, 1), " (",
                                       round(q25, 1), ", ",
                                       round(q75, 1), ")")]

  }
}

rm(med_col, q25_col, q75_col, text_col, med, q25, q75, abs_col, cur_chc, i,
   n, n_col)

# now delete incongruent values
for (i in 1:nrow(abs_data)) {
  cur_yr <- abs_data[i, exposure]
  cur_yr <- gsub("[a-z]|_", "", cur_yr)
  cols_to_replace <- colnames(abs_data)
  cols_to_replace <- cols_to_replace[!(grepl(paste0("_", as.integer(cur_yr) + 1, "_"), cols_to_replace))]
  cols_to_replace <- cols_to_replace[!(cols_to_replace %in% c("exposure", "exposure_collapse"))]
  abs_data[i, ((cols_to_replace)) := NA]
}

rm(i, cur_yr, cols_to_replace)

for (i in 3:ncol(abs_data)) {
  current_col <- names(abs_data)[i]
  abs_data[, (current_col) := get(current_col)[!is.na(get(current_col))], by = .(exposure_collapse)]
}

rm(i, current_col)

abs_data[, exposure := NULL]
abs_data <- abs_data[!duplicated(abs_data)]

write.csv(abs_data, file = "3_DESCRIPTIVE_STUDY/outputs/Table 2 - percent health.csv",
          row.names = F)

rm(abs_data)


