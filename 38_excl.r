# excl  -----------------------------------------------------
excl_cols <- excl_cols <- c("ever_excluded",
                           paste0("excluded_y", 7:11))

# overall -----------------------------------------------------------------

excl_data <- data.table(
  exposure = chc_cols,
  exposure_collapse = rep(chc_groups, each = 5)
)

for (i in 1:nrow(excl_data)) {
  
  cur_chc <- excl_data[i, exposure]
  print(cur_chc)
  
  for (excl_col in excl_cols) {

    t <- table(cohort_eligible[get(cur_chc) == T, get(excl_col)])
    n <- t[2]
    d <- sum(t)
    p <- round((n / d) * 100, 1)
    text_col <- paste0(excl_col, "_n_p")
    excl_data[i, ((text_col)) := paste0(n, " (", p, "%)")]
    
  }
  
}

# now delete incongruent values
for (i in 1:nrow(excl_data)) {
  
  cur_yr <- excl_data[i, exposure]
  cur_yr <- gsub("[a-z]|_", "", cur_yr)
  
  cols_to_replace <- colnames(excl_data)
  cols_to_replace <- cols_to_replace[!(grepl(paste0("_y", as.integer(cur_yr) + 1, "_"), cols_to_replace))]
  
  if (cur_yr == 6) {
    cols_to_replace <- cols_to_replace[!(grepl("exposure|ever", cols_to_replace))]
  } else {
    cols_to_replace <- cols_to_replace[!(grepl("exposure", cols_to_replace))]
  }
  
  excl_data[i, ((cols_to_replace)) := NA] 
  
}

for (i in 3:ncol(excl_data)) {
  current_col <- names(excl_data)[i]
  excl_data[, (current_col) := get(current_col)[!is.na(get(current_col))], by = .(exposure_collapse)]
}

excl_data[, exposure := NULL]
excl_data <- excl_data[!duplicated(excl_data)]

write.csv(excl_data, file = "3_DESCRIPTIVE_STUDY/outputs/Table 6 - excluded.csv",
          row.names = F)

rm(t, n, p, text_col, excl_col, cur_chc, i, d, p_unexp,
   p_all, parf_col, current_col, cur_yr, cols_to_replace, excl_data)
