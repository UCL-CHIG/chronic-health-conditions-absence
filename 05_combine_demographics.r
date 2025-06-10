
print("Combining and saving")
load("3_DESCRIPTIVE_STUDY/processed/demographics_modals.rda")
load("3_DESCRIPTIVE_STUDY/processed/demographics_y7.rda")
load("3_DESCRIPTIVE_STUDY/processed/demographics_ever.rda")

print("Demographics modals")
cohort_spine <- merge(cohort_spine,
                      demo_modals,
                      by = "PupilMatchingRefAnonymous",
                      all.x = T)

# table(cohort_spine$female, useNA = "always")
# table(cohort_spine$ethnicgroup, useNA = "always")
# table(cohort_spine$languagegroup, useNA = "always")
# table(cohort_spine$yearofbirth, useNA = "always")
# table(cohort_spine$monthofbirth, useNA = "always")

cohort_spine[, dob_approx := as.Date(paste0(yearofbirth, "-", monthofbirth, "-15"))]


print("Demographics 7y")
cohort_spine <- merge(cohort_spine,
                      demo_y7,
                      by = "PupilMatchingRefAnonymous",
                      all.x = T)

# table(cohort_spine$fsmeligible_y7, useNA = "always")
# table(cohort_spine$idaci_decile_y7, useNA = "always")


print("Demographics ever")
cohort_spine[, fsm_ever_primary := PupilMatchingRefAnonymous %in%
               demographics_ever[fsm_ever_primary == 1]$PupilMatchingRefAnonymous]

cohort_spine[, highest_sen_primary := factor("None",
                                             levels = c("None",
                                                        "Support",
                                                        "S/EHCP",
                                                        "Specialist provision"))]

cohort_spine[PupilMatchingRefAnonymous %in% demographics_ever[specialist_school == T]$PupilMatchingRefAnonymous,
             highest_sen_primary := "Specialist provision"]

cohort_spine[highest_sen_primary != "Specialist provision" &
               PupilMatchingRefAnonymous %in% demographics_ever[senprovision == "sehcp"]$PupilMatchingRefAnonymous,
             highest_sen_primary := "S/EHCP"]

cohort_spine[highest_sen_primary != "Specialist provision" & highest_sen_primary != "S/EHCP" &
               PupilMatchingRefAnonymous %in% demographics_ever[senprovision == "support"]$PupilMatchingRefAnonymous,
             highest_sen_primary := "Support"]

cohort_spine[, ever_sen_primary := ifelse(highest_sen_primary == "None", F, T)]

table(cohort_spine$highest_sen_primary, useNA = "always")
table(cohort_spine$ever_sen_primary, useNA = "always")
table(cohort_spine$fsm_ever_primary, useNA = "always")



print("Creating eligibilty flag")

cohort_spine[, born_in_year := dob_approx >= as.Date("2000-09-01") & dob_approx <= as.Date("2003-08-31")]
cohort_spine[, eligible := enrolled_ks1 & !is.na(encrypted_hesid) & born_in_year]
cohort_spine <- cohort_spine[order(PupilMatchingRefAnonymous, inception_year)]



save(cohort_spine, file = "3_DESCRIPTIVE_STUDY/processed/1_cohort_spine.rda")
rm(list = ls()); gc()
