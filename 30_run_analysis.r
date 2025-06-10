
setwd("[path_omitted]")
assign(".lib.loc", c(.libPaths(), "[path_omitted]"), envir = environment(.libPaths))

library(data.table)
library(gtsummary)
library(gt)
library(ggplot2)
library(gridExtra)

load("3_DESCRIPTIVE_STUDY/processed/2_cohort_spine_complete.rda")

source("3_DESCRIPTIVE_STUDY/scripts/31a_additional_cleaning.r")
source("3_DESCRIPTIVE_STUDY/scripts/31b_select_chcs.r")

source("3_DESCRIPTIVE_STUDY/scripts/32_characteristics.r")
source("3_DESCRIPTIVE_STUDY/scripts/33a_numbers_excluded.r")
source("3_DESCRIPTIVE_STUDY/scripts/33b_missing_absence_by_chc.r")
source("3_DESCRIPTIVE_STUDY/scripts/34_cumulative_prev_chc.r")

source("3_DESCRIPTIVE_STUDY/scripts/35_perc_health.r")
source("3_DESCRIPTIVE_STUDY/scripts/36_pers_abs.r")
source("3_DESCRIPTIVE_STUDY/scripts/37_enrol.r")
source("3_DESCRIPTIVE_STUDY/scripts/38_excl.r")
source("3_DESCRIPTIVE_STUDY/scripts/39_number_outcomes.r")

rm(list = ls())
