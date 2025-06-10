
# Excluding in order
table(!cohort_spine$born_in_year | is.na(cohort_spine$born_in_year))
table(cohort_spine[born_in_year == T]$enrolled_ks1)
table(is.na(cohort_spine[born_in_year == T & enrolled_ks1 == T]$encrypted_hesid))

output <-
  data.table(
    excl_reason = c("Not born in year",
                    "Not enrolled in KS1",
                    "No HES link"),
    n = as.integer(NA),
    p = as.double(NA),
    np = as.character(NA)
  )

d_new <- nrow(cohort_spine)
n_new <- sum(cohort_spine$born_in_year == F | is.na(cohort_spine$born_in_year))
p_new <- round((n_new / d_new) * 100, 1)
np_new <- paste0(n_new, " (", p_new, "%)")

output[1, n := n_new]
output[1, p := p_new]
output[1, np := np_new]


d_new <- nrow(cohort_spine[born_in_year == T])
n_new <- sum(cohort_spine[born_in_year == T]$enrolled_ks1 == F)
p_new <- round((n_new / d_new) * 100, 1)
np_new <- paste0(n_new, " (", p_new, "%)")

output[2, n := n_new]
output[2, p := p_new]
output[2, np := np_new]



d_new <- nrow(cohort_spine[born_in_year == T & enrolled_ks1 == T])
n_new <- sum(is.na(cohort_spine[born_in_year == T & enrolled_ks1 == T]$encrypted_hesid))
p_new <- round((n_new / d_new) * 100, 1)
np_new <- paste0(n_new, " (", p_new, "%)")

output[3, n := n_new]
output[3, p := p_new]
output[3, np := np_new]



write.csv(output, file = "3_DESCRIPTIVE_STUDY/outputs/Table 0 - exclusions.csv", row.names = F)

rm(output, d_new, n_new, p_new, np_new)



# numbers by year ---------------------------------------------------------

cohort_eligible[, outcome_ind_y7 := factor("Not enrolled, no absence data",
                                           levels = c("Not enrolled, no absence data",
                                                      "Not enrolled, absence data",
                                                      "Enrolled, no absence data",
                                                      "Enrolled, absence data"))]
cohort_eligible[is.na(abs_overall_rate_7), outcome_ind_y7 := "Enrolled, no absence data"]
cohort_eligible[!is.na(abs_overall_rate_7), outcome_ind_y7 := "Enrolled, absence data"]

for (i in 8:11) {
  new_var <- paste0("outcome_ind_y", i)
  enr_var <- paste0("not_enrolled_y", i)
  abs_var <- paste0("abs_overall_rate_", i)
  
  cohort_eligible[, (new_var) := factor("Not enrolled, no absence data",
                                        levels = c("Not enrolled, no absence data",
                                                   "Not enrolled, absence data",
                                                   "Enrolled, no absence data",
                                                   "Enrolled, absence data"))]
  cohort_eligible[get(enr_var) == T & !is.na(get(abs_var)), (new_var) := "Not enrolled, absence data"]
  cohort_eligible[get(enr_var) == F & is.na(get(abs_var)), (new_var) := "Enrolled, no absence data"]
  cohort_eligible[get(enr_var) == F & !is.na(get(abs_var)), (new_var) := "Enrolled, absence data"]
  
}

output <- data.table(
  status = c("Not enrolled, no absence data",
             "Not enrolled, absence data",
             "Enrolled, no absence data",
             "Enrolled, absence data"),
  y7 = as.character(NA),
  y8 = as.character(NA),
  y9 = as.character(NA),
  y10 = as.character(NA),
  y11 = as.character(NA)
)

for (i in 7:11) {
  out_var <- paste0("outcome_ind_y", i)
  y_var <- paste0("y", i)
  lev <- levels(cohort_eligible$outcome_ind_y8)
  
  n <- table(cohort_eligible[, get(out_var)])[levels(cohort_eligible$outcome_ind_y8)]
  p <- round(prop.table(n) * 100, 1)
  np <- paste0(n, " (", p, "%)")
  
  output[, (y_var) :=  np]
}

output

write.csv(output, file = "3_DESCRIPTIVE_STUDY/outputs/Table 0a - has absence data.csv", row.names = F)

rm(new_var, enr_var, abs_var, out_var, y_var, lev, i, n, p, np, output)

# add sessions
output <- data.table(
  status = c("Not enrolled, absence data",
             "Enrolled, absence data"),
  y7_sessions = as.character(NA),
  y8_sessions = as.character(NA),
  y9_sessions = as.character(NA),
  y10_sessions = as.character(NA),
  y11_sessions = as.character(NA)
)

med <- aggregate(cohort_eligible[, sessionspossible_annual_7] ~
                   cohort_eligible[, outcome_ind_y7],
                 FUN = function(x) paste0(median(x), " (", quantile(x, 0.25), ", ", quantile(x, 0.75), ")"))[2]

output[2, y7_sessions := med]

for (i in 8:11) {
  ses_var <- paste0("sessionspossible_annual_", i)
  y_var <- paste0("y", i, "_sessions")
  status_var <- paste0("outcome_ind_y", i)
  enr_var <- paste0("not_enrolled_y", i)
  
  med <- aggregate(cohort_eligible[, get(ses_var)] ~ cohort_eligible[, get(status_var)],
                   data = cohort_eligible[get(enr_var) == F],
                   FUN = function(x) paste0(median(x), " (", quantile(x, 0.25), ", ", quantile(x, 0.75), ")"))[2]

  output[, (y_var) :=  med]
}

output

write.csv(output, file = "3_DESCRIPTIVE_STUDY/outputs/Table 0b - sessions possible.csv", row.names = F)

rm(enr_var, y_var, ses_var, status_var, med, i, output)
