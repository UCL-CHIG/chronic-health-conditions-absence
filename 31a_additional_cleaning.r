
cohort_spine[, any_missing :=
               is.na(female) |
               is.na(ethnicgroup) |
               is.na(languagegroup) |
               is.na(fsmeligible_y7) |
               is.na(idaci_decile_y7) |
               is.na(fsm_ever_primary)]

# Create eligible dataset
cohort_eligible <- cohort_spine[eligible == T]

# idaci and fsm
cohort_eligible[, idaci_decile_y7 := factor(idaci_decile_y7)]

cohort_eligible[, idaci_quint_y7 := factor(NA, levels = c(5:1))]
cohort_eligible[idaci_decile_y7 %in% 10:9, idaci_quint_y7 := 5]
cohort_eligible[idaci_decile_y7 %in% 8:7, idaci_quint_y7 := 4]
cohort_eligible[idaci_decile_y7 %in% 6:5, idaci_quint_y7 := 3]
cohort_eligible[idaci_decile_y7 %in% 4:3, idaci_quint_y7 := 2]
cohort_eligible[idaci_decile_y7 %in% 2:1, idaci_quint_y7 := 1]

cohort_eligible[, idaci_fsm_quint_y7 := factor(NA, levels = c("5,0",
                                                              "5,1",
                                                              "4,0",
                                                              "4,1",
                                                              "3,0",
                                                              "3,1",
                                                              "2,0",
                                                              "2,1",
                                                              "1,0",
                                                              "1,1"))]

cohort_eligible[idaci_quint_y7 == 1 & fsmeligible_y7 == 1, idaci_fsm_quint_y7 := "1,1"]
cohort_eligible[idaci_quint_y7 == 1 & fsmeligible_y7 == 0, idaci_fsm_quint_y7 := "1,0"]
cohort_eligible[idaci_quint_y7 == 2 & fsmeligible_y7 == 1, idaci_fsm_quint_y7 := "2,1"]
cohort_eligible[idaci_quint_y7 == 2 & fsmeligible_y7 == 0, idaci_fsm_quint_y7 := "2,0"]
cohort_eligible[idaci_quint_y7 == 3 & fsmeligible_y7 == 1, idaci_fsm_quint_y7 := "3,1"]
cohort_eligible[idaci_quint_y7 == 3 & fsmeligible_y7 == 0, idaci_fsm_quint_y7 := "3,0"]
cohort_eligible[idaci_quint_y7 == 4 & fsmeligible_y7 == 1, idaci_fsm_quint_y7 := "4,1"]
cohort_eligible[idaci_quint_y7 == 4 & fsmeligible_y7 == 0, idaci_fsm_quint_y7 := "4,0"]
cohort_eligible[idaci_quint_y7 == 5 & fsmeligible_y7 == 1, idaci_fsm_quint_y7 := "5,1"]
cohort_eligible[idaci_quint_y7 == 5 & fsmeligible_y7 == 0, idaci_fsm_quint_y7 := "5,0"]

# unauth abs
cohort_eligible[, abs_unauth_rate_7_bin := ifelse(abs_unauth_rate_7 > 0, T, F)]
cohort_eligible[, abs_unauth_rate_8_bin := ifelse(abs_unauth_rate_8 > 0, T, F)]
cohort_eligible[, abs_unauth_rate_9_bin := ifelse(abs_unauth_rate_9 > 0, T, F)]
cohort_eligible[, abs_unauth_rate_10_bin := ifelse(abs_unauth_rate_10 > 0, T, F)]
cohort_eligible[, abs_unauth_rate_11_bin := ifelse(abs_unauth_rate_11 > 0, T, F)]

cohort_eligible[, abs_unauth_rate_7_bin_5 := abs_unauth_rate_7 > 5]
cohort_eligible[, abs_unauth_rate_8_bin_5 := abs_unauth_rate_8 > 5]
cohort_eligible[, abs_unauth_rate_9_bin_5 := abs_unauth_rate_9 > 5]
cohort_eligible[, abs_unauth_rate_10_bin_5 := abs_unauth_rate_10 > 5]
cohort_eligible[, abs_unauth_rate_11_bin_5 := abs_unauth_rate_11 > 5]

# sessions possible
cohort_eligible[is.na(abs_auth_rate_7), sessionspossible_annual_7 := NA]
cohort_eligible[is.na(abs_auth_rate_8), sessionspossible_annual_8 := NA]
cohort_eligible[is.na(abs_auth_rate_9), sessionspossible_annual_9 := NA]
cohort_eligible[is.na(abs_auth_rate_10), sessionspossible_annual_10 := NA]
cohort_eligible[is.na(abs_auth_rate_11), sessionspossible_annual_11 := NA]


# merge CHC groups
for (i in 6:10) {
  new_mh_var <- paste0("chc_mh_", i)
  ara <- paste0("chc_ara_", i)
  srp <- paste0("chc_srp_psych_", i)
  int <- paste0("chc_srp_internalising_", i)
  ext <- paste0("chc_srp_externalising_", i)

  cohort_eligible[, (new_mh_var) :=
                    get(ara) == T |
                    get(srp) == T |
                    get(int) == T |
                    get(ext) == T]

}

rm(ara, srp, int, ext, i, new_mh_var)

