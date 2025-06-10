
# out_data <- data.frame(
#   exposure = chc_groups,
#   n0 = as.character(NA),
#   n1 = as.character(NA),
#   n2 = as.character(NA),
#   n3 = as.character(NA),
#   missing = as.character(NA)
# )
# 
# for (chc in chc_groups) {
#   
#   chc_col <- paste0("chc_", chc, "_6")
#   t <- table(cohort_eligible[get(chc_col) == T]$n_outcomes_ever_8to11, useNA = "always")
#   print(t)
#   out_data[out_data$exposure == chc, 2:6] <- t
#   
# }
# 
# # explore the missing
# table(n_out = is.na(cohort_eligible$n_outcomes_ever_8to11), is.na(cohort_eligible$ever_pa10_8to11))
# table(n_out = is.na(cohort_eligible$n_outcomes_ever_8to11), is.na(cohort_eligible$ever_excluded_8to11))
# table(n_out = is.na(cohort_eligible$n_outcomes_ever_8to11), is.na(cohort_eligible$ever_not_enrolled))
# 
# cohort_eligible[, pa_ever_missing :=
#                   is.na(pa10_8) |
#                   is.na(pa10_9) |
#                   is.na(pa10_10) |
#                   is.na(pa10_11)]
# table(cohort_eligible$pa_ever_missing)
# table(n_out = is.na(cohort_eligible$n_outcomes_ever_8to11), pa_ever = cohort_eligible$pa_ever_missing)
# 
# cohort_eligible[, pa_always_missing := 
#                   is.na(pa10_8) &
#                   is.na(pa10_9) &
#                   is.na(pa10_10) &
#                   is.na(pa10_11)]
# table(cohort_eligible$pa_always_missing)
# table(n_out = is.na(cohort_eligible$n_outcomes_ever_8to11), pa_always = cohort_eligible$pa_always_missing)

# n outcomes is missing if PA is missing on all of years 8 to 11 OR
# if some are missing but the remainder are false.
# In other words, trues are always picked up.
# Re-do just with the trues.

out_data <- data.frame(
  exposure = chc_groups,
  n0 = as.integer(NA),
  n1 = as.integer(NA),
  n2 = as.integer(NA),
  n3 = as.integer(NA)
)

for (chc in chc_groups) {
  
  chc_col <- paste0("chc_", chc, "_6")
  t <- table(cohort_eligible[get(chc_col) == T]$n_outcomes_ever_8to11)
  print(t)
  out_data[out_data$exposure == chc, 2:5] <- t
  
}

out_data <- data.table(out_data)
out_data[, total := n0 + n1 + n2 + n3]

out_data[, p0 := (n0 / total) * 100]
out_data[, p1ormore := ((n1 + n2 + n3) / total) * 100]
out_data[, p1 := (n1 / total) * 100]
out_data[, p2 := (n2 / total) * 100]
out_data[, p3 := (n3 / total) * 100]

out_data[, np0 := paste0(n0, " (", round(p0, 1), "%)")]
out_data[, np1ormore := paste0((n1 + n2 + n3), " (", round(p1ormore, 1), "%)")]
out_data[, np1 := paste0(n1, " (", round(p1, 1), "%)")]
out_data[, np2 := paste0(n2, " (", round(p2, 1), "%)")]
out_data[, np3 := paste0(n3, " (", round(p3, 1), "%)")]

out_data <- out_data[, c("exposure", "np0", "np1ormore", paste0("np", 1:3))]

write.csv(out_data, file = "3_DESCRIPTIVE_STUDY/outputs/Table 7 - n outcomes.csv", row.names = F)
