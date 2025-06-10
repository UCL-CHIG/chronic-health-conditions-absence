char_cols <- c("female",
               "ethnicgroup",
               "languagegroup",
               "fsmeligible_y7",
               "idaci_quint_y7",
               "idaci_fsm_quint_y7")

table1a <-
  tbl_summary(
    cohort_eligible,
    include = char_cols,
    by = "inception_year"
  )

table1b <-
  tbl_summary(
    cohort_eligible,
    include = char_cols
  )

table1 <-
  tbl_merge(
    tbls = list(table1a, table1b),
    tab_spanner = c("**Inception year**", "**Total**")
  )

gtsave(as_gt(table1), file = "3_DESCRIPTIVE_STUDY/outputs/Table 1 - demographics.rtf")
rm(table1, table1a, table1b, char_cols)
