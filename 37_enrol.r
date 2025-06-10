
# enr_cpols -----------------------------------------------------

enr_cols <- c("ever_not_enrolled",
              paste0("not_enrolled_y", 8:11))

# overall -----------------------------------------------------------------

enr_data <- data.table(
  exposure = chc_cols[!(grepl("_6", chc_cols))],
  exposure_collapse = rep(chc_groups, each = 4)
)

for (i in 1:nrow(enr_data)) {
  
  cur_chc <- enr_data[i, exposure]
  print(cur_chc)
  
  for (enr_col in enr_cols) {
  
    t <- table(cohort_eligible[get(cur_chc) == T, get(enr_col)])
    n <- t[2]
    d <- sum(t)
    p <- round((n / d) * 100, 1)

    text_col <- paste0(enr_col, "_n_p")
    enr_data[i, ((text_col)) := paste0(n, " (", p, "%)")]
    
  }

}

# now delete incongruent values
for (i in 1:nrow(enr_data)) {
  
  cur_yr <- enr_data[i, exposure]
  cur_yr <- gsub("[a-z]|_", "", cur_yr)
  
  cols_to_replace <- colnames(enr_data)
  cols_to_replace <- cols_to_replace[!(grepl(paste0("_y", as.integer(cur_yr) + 1, "_"), cols_to_replace))]
  
  if (cur_yr == 7) {
    cols_to_replace <- cols_to_replace[!(grepl("exposure|ever", cols_to_replace))]
  } else {
    cols_to_replace <- cols_to_replace[!(grepl("exposure", cols_to_replace))]
  }
  
  enr_data[i, ((cols_to_replace)) := NA] 
  
}

for (i in 3:ncol(enr_data)) {
  current_col <- names(enr_data)[i]
  enr_data[, (current_col) := get(current_col)[!is.na(get(current_col))], by = .(exposure_collapse)]
}

enr_data[, exposure := NULL]
enr_data <- enr_data[!duplicated(enr_data)]

write.csv(enr_data, file = "3_DESCRIPTIVE_STUDY/outputs/Table 5 - unenrolment.csv",
          row.names = F)


rm(t, n, p, text_col, enr_col, cur_chc, i, d, p_unexp,
   p_all, parf_col, current_col, cur_yr, cols_to_replace, enr_data)


