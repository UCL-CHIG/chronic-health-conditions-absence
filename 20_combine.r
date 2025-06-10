
print("Creating flags and merging data")

print("CHC exposure")

chc_groups <- names(table(chc_codes$chc_group))

for (chc in chc_groups) {
  
  for (year_end in 6:10) {
    
    new_col <- paste0("chc_", chc, "_", year_end)
    print(paste0("Now doing ", new_col))
    
    cohort_spine[, ((new_col)) := as.logical(NA)]
    
    for (iy in 2013:2015) {
      cohort_spine[inception_year == iy, ((new_col)) := encrypted_hesid %in%
                     chc_codes[chc_group == chc &
                                 inception_year == iy &
                                 epistart < as.Date(paste0(iy + year_end - 7, "-09-01"))]$encrypted_hesid]
    }
  }
}

print("Any condition groups")

for (year_end in 6:10) {
  
  new_col <- paste0("chc_any_", year_end)
  hardelid <- paste0("chc_hardelid_", year_end)
  neurodis <- paste0("chc_neurodis_", year_end)
  srp_int <- paste0("chc_srp_internalising_", year_end)
  srp_ext <- paste0("chc_srp_externalising_", year_end)
  srp_psych <- paste0("chc_srp_psych_", year_end)
  ara <- paste0("chc_ara_", year_end)
  
  print(paste0("Now doing ", new_col))
  
  cohort_spine[, ((new_col)) :=
                 get(hardelid) == T |
                 get(neurodis) == T |
                 get(srp_int) == T |
                 get(srp_ext) == T |
                 get(srp_psych) == T |
                 get(ara) == T]
  
}


print("Non-enrolment")
enrolments_spring <- enrolments[census %in% c("Spring", "AP")]

cohort_spine[, not_enrolled_y8 := !(PupilMatchingRefAnonymous %in% enrolments_spring[year == 8]$PupilMatchingRefAnonymous)]
cohort_spine[, not_enrolled_y9 := !(PupilMatchingRefAnonymous %in% enrolments_spring[year == 9]$PupilMatchingRefAnonymous)]
cohort_spine[, not_enrolled_y10 := !(PupilMatchingRefAnonymous %in% enrolments_spring[year == 10]$PupilMatchingRefAnonymous)]
cohort_spine[, not_enrolled_y11 := !(PupilMatchingRefAnonymous %in% enrolments_spring[year == 11]$PupilMatchingRefAnonymous)]

cohort_spine[, ever_not_enrolled := not_enrolled_y8 | not_enrolled_y9 | not_enrolled_y10 | not_enrolled_y11]
cohort_spine[, not_enrolled_n_yr := sum(not_enrolled_y8,
                                        not_enrolled_y9,
                                        not_enrolled_y10,
                                        not_enrolled_y11), by = PupilMatchingRefAnonymous]



print("Exclusions")
cohort_spine[, excluded_y7 := PupilMatchingRefAnonymous %in% exclusions[year == 7]$PupilMatchingRefAnonymous]
cohort_spine[, excluded_y8 := PupilMatchingRefAnonymous %in% exclusions[year == 8]$PupilMatchingRefAnonymous]
cohort_spine[, excluded_y9 := PupilMatchingRefAnonymous %in% exclusions[year == 9]$PupilMatchingRefAnonymous]
cohort_spine[, excluded_y10 := PupilMatchingRefAnonymous %in% exclusions[year == 10]$PupilMatchingRefAnonymous]
cohort_spine[, excluded_y11 := PupilMatchingRefAnonymous %in% exclusions[year == 11]$PupilMatchingRefAnonymous]

cohort_spine[, ever_excluded := excluded_y7 | excluded_y8 | excluded_y9 | excluded_y10 | excluded_y11]


print("Absence")
absence_rates <- dcast(absence,
                       PupilMatchingRefAnonymous ~ year,
                       value.var = c("abs_auth_rate", "abs_unauth_rate",
                                     "abs_overall_rate", "pa10",
                                     "abs_illness_rate",
                                     "abs_appts_rate",
                                     "abs_illness_appts_rate",
                                     "sessionspossible_annual"))

cohort_spine <- merge(cohort_spine,
                      absence_rates,
                      by = "PupilMatchingRefAnonymous",
                      all.x = T)

# Ever PA
cohort_spine[, ever_pa10 := pa10_7 | pa10_8 | pa10_9 | pa10_10 | pa10_11]

# % of absence that is health-related (illness_appts_rate / overall_rate)
for (year in 7:11) {
  
  new_col <- paste0("perc_health_abs_", year)
  ove_col <- paste0("abs_overall_rate_", year)
  illabs_col <- paste0("abs_illness_appts_rate_", year)
  
  cohort_spine[, ((new_col)) := as.double(NA)]
  
  cohort_spine[get(ove_col) > 0, ((new_col)) := (get(illabs_col) / get(ove_col)) * 100]
}

# number of outcomes - ever
cohort_spine[, ever_pa10_8to11 := pa10_8 | pa10_9 | pa10_10 | pa10_11]
cohort_spine[, ever_excluded_8to11 := excluded_y8 | excluded_y9 | excluded_y10 | excluded_y11]
cohort_spine[, n_outcomes_ever_8to11 := ever_pa10_8to11 + ever_excluded_8to11 + ever_not_enrolled]

# special schools
print("Special schools")
for (i in 7:11) {
  
  new_col <- paste0("special_", i)
  cohort_spine[, ((new_col)) := PupilMatchingRefAnonymous %in% special_schools[year == i]$PupilMatchingRefAnonymous]
  
}

# save
print("Saving and clearing memory")
save(cohort_spine, file = "3_DESCRIPTIVE_STUDY/processed/2_cohort_spine_complete.rda")
rm(list = ls()); gc()
