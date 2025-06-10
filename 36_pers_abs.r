# abs_cols ----------------------------------------------------------------

abs_cols <- names(cohort_eligible)[grepl("pa10_", names(cohort_eligible))]

# overall -----------------------------------------------------------------

abs_data <- data.table(
  exposure = chc_cols,
  exposure_collapse = rep(chc_groups, each = 5)
)

for (i in 1:nrow(abs_data)) {
  
  cur_chc <- abs_data[i, exposure]
  print(cur_chc)
  
  for (abs_col in abs_cols) {
    
    t <- table(cohort_eligible[get(cur_chc) == T, get(abs_col)])
    n <- t[2]
    d <- sum(t)
    p <- round((n / d) * 100, 1)
    text_col <- paste0(abs_col, "_n_p")
    
    abs_data[i, ((text_col)) := paste0(n, " (", p, "%)")]
    
  }
}

# now delete incongruent values
for (i in 1:nrow(abs_data)) {
  
  cur_yr <- abs_data[i, exposure]
  cur_yr <- gsub("[a-z]|_", "", cur_yr)
  
  cols_to_replace <- colnames(abs_data)
  cols_to_replace <- cols_to_replace[!(grepl(paste0("_", as.integer(cur_yr) + 1, "_"), cols_to_replace))]
  
  if (cur_yr == 6) {
    cols_to_replace <- cols_to_replace[!(grepl("exposure|ever", cols_to_replace))]
  } else {
    cols_to_replace <- cols_to_replace[!(grepl("exposure", cols_to_replace))]
  }
  
  abs_data[i, ((cols_to_replace)) := NA] 
  
}

for (i in 3:ncol(abs_data)) {
  current_col <- names(abs_data)[i]
  abs_data[, (current_col) := get(current_col)[!is.na(get(current_col))], by = .(exposure_collapse)]
}

abs_data[, exposure := NULL]
abs_data <- abs_data[!duplicated(abs_data)]


write.csv(abs_data, file = "3_DESCRIPTIVE_STUDY/outputs/Table 4 - persistent absence.csv",
          row.names = F)

rm(t, n, p, text_col, abs_col, cur_chc, i, d, p_unexp,
   p_all, parf_col, current_col, cur_yr, cols_to_replace, abs_data)

