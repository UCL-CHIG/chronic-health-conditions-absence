
abs_cols <- names(cohort_eligible)[grepl("pa10_", names(cohort_eligible))]
abs_cols <- abs_cols[abs_cols != "ever_pa10_8to11"]
enr_cols <- paste0("not_enrolled_y", 8:11)
excl_cols <- paste0("excluded_y", 7:11)
special_cols <- paste0("special_", 7:11)

abs_data <- data.table(
  exposure = chc_cols,
  exposure_collapse = rep(chc_groups, each = 5)
)

for (i in 1:nrow(abs_data)) {
  
  cur_chc <- abs_data[i, exposure]
  print(cur_chc)
  
  for (j in 1:length(abs_cols)) {
    
    if (j == 1) {
      
      abs_col <- abs_cols[j]
      excl_col <- excl_cols[j]
      special_col <- special_cols[j]
      
      # overall
      t <- table(is.na(cohort_eligible[get(cur_chc) == T, get(abs_col)]))
      n <- t[2]
      d <- sum(t)
      p <- round((n / d) * 100, 1)
      text_col <- paste0("ove_", abs_col)
      
      abs_data[i, ((text_col)) := paste0(n, " (", p, "%)")]
      
      # excluded
      t <- table(is.na(cohort_eligible[get(cur_chc) == T & get(excl_col) == T, get(abs_col)]))
      n <- t[2]
      d <- sum(t)
      p <- round((n / d) * 100, 1)
      text_col <- paste0("excl_", abs_col)
      
      abs_data[i, ((text_col)) := paste0(n, " (", p, "%)")]
      
      t <- table(is.na(cohort_eligible[get(cur_chc) == T & get(excl_col) == F, get(abs_col)]))
      n <- t[2]
      d <- sum(t)
      p <- round((n / d) * 100, 1)
      text_col <- paste0("not_excl_", abs_col)
      
      abs_data[i, ((text_col)) := paste0(n, " (", p, "%)")]
      
      # special school
      t <- table(is.na(cohort_eligible[get(cur_chc) == T & get(special_col) == T, get(abs_col)]))
      n <- t[2]
      d <- sum(t)
      p <- round((n / d) * 100, 1)
      text_col <- paste0("special_", abs_col)
      
      abs_data[i, ((text_col)) := paste0(n, " (", p, "%)")]
      
      t <- table(is.na(cohort_eligible[get(cur_chc) == T & get(special_col) == F, get(abs_col)]))
      n <- t[2]
      d <- sum(t)
      p <- round((n / d) * 100, 1)
      text_col <- paste0("not_special_", abs_col)
      
      abs_data[i, ((text_col)) := paste0(n, " (", p, "%)")]
      
    } else {
      
      abs_col <- abs_cols[j]
      enr_col <- enr_cols[j - 1]
      excl_col <- excl_cols[j]
      special_col <- special_cols[j]
      
      # overall
      t <- table(is.na(cohort_eligible[get(cur_chc) == T, get(abs_col)]))
      n <- t[2]
      d <- sum(t)
      p <- round((n / d) * 100, 1)
      text_col <- paste0("ove_", abs_col)
      
      abs_data[i, ((text_col)) := paste0(n, " (", p, "%)")]
      
      # not enrolled
      t <- table(is.na(cohort_eligible[get(cur_chc) == T & get(enr_col) == T, get(abs_col)]))
      n <- t[2]
      d <- sum(t)
      p <- round((n / d) * 100, 1)
      text_col <- paste0("not_enr_", abs_col)
      
      abs_data[i, ((text_col)) := paste0(n, " (", p, "%)")]
      
      t <- table(is.na(cohort_eligible[get(cur_chc) == T & get(enr_col) == F, get(abs_col)]))
      n <- t[2]
      d <- sum(t)
      p <- round((n / d) * 100, 1)
      text_col <- paste0("enr_", abs_col)
      
      abs_data[i, ((text_col)) := paste0(n, " (", p, "%)")]
      
      # excluded
      t <- table(is.na(cohort_eligible[get(cur_chc) == T & get(excl_col) == T, get(abs_col)]))
      n <- t[2]
      d <- sum(t)
      p <- round((n / d) * 100, 1)
      text_col <- paste0("excl_", abs_col)
      
      abs_data[i, ((text_col)) := paste0(n, " (", p, "%)")]
      
      t <- table(is.na(cohort_eligible[get(cur_chc) == T & get(excl_col) == F, get(abs_col)]))
      n <- t[2]
      d <- sum(t)
      p <- round((n / d) * 100, 1)
      text_col <- paste0("not_excl_", abs_col)
      
      abs_data[i, ((text_col)) := paste0(n, " (", p, "%)")]
      
      # special school
      t <- table(is.na(cohort_eligible[get(cur_chc) == T & get(special_col) == T, get(abs_col)]))
      n <- t[2]
      d <- sum(t)
      p <- round((n / d) * 100, 1)
      text_col <- paste0("special_", abs_col)
      
      abs_data[i, ((text_col)) := paste0(n, " (", p, "%)")]
      
      t <- table(is.na(cohort_eligible[get(cur_chc) == T & get(special_col) == F, get(abs_col)]))
      n <- t[2]
      d <- sum(t)
      p <- round((n / d) * 100, 1)
      text_col <- paste0("not_special_", abs_col)
      
      abs_data[i, ((text_col)) := paste0(n, " (", p, "%)")]
      
    }
    
  }
}

for (i in 1:nrow(abs_data)) {
  
  cur_yr <- abs_data[i, exposure]
  cur_yr <- gsub("[a-z]|_", "", cur_yr)
  
  cols_to_replace <- colnames(abs_data)
  cols_to_replace <- cols_to_replace[!(grepl(paste0("_", as.integer(cur_yr) + 1), cols_to_replace))]
  cols_to_replace <- cols_to_replace[!(grepl("exposure", cols_to_replace))]

  abs_data[i, ((cols_to_replace)) := NA] 
  
}

for (i in 3:ncol(abs_data)) {
  current_col <- names(abs_data)[i]
  abs_data[, (current_col) := get(current_col)[!is.na(get(current_col))], by = .(exposure_collapse)]
}

abs_data[, exposure := NULL]
abs_data <- abs_data[!duplicated(abs_data)]

write.csv(abs_data, file = "3_DESCRIPTIVE_STUDY/outputs/Table x1 - missing absence.csv",
          row.names = F)

rm(t, n, p, j, text_col, abs_col, cur_chc, i, d,
   current_col, cur_yr, cols_to_replace, abs_data,
   excl_cols, excl_col, enr_cols, enr_col)

