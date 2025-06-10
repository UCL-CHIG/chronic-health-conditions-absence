setwd("[path_omitted]")
assign(".lib.loc", c(.libPaths(), "[path_omitted]"), envir = environment(.libPaths))

library(data.table)
library(RODBC)
library(dplyr)
library(bit64)
library(stringr)
library(readxl)

rm(list = ls()); gc()
master_dir <- "[path_omitted]"
birth_cohorts <- 2001:2003

source("3_DESCRIPTIVE_STUDY/scripts/01_identify_cohort.r")
source("3_DESCRIPTIVE_STUDY/scripts/02_demographic_modals.r")
source("3_DESCRIPTIVE_STUDY/scripts/03_demographic_year7.r")
source("3_DESCRIPTIVE_STUDY/scripts/04_demographic_ever.r")
source("3_DESCRIPTIVE_STUDY/scripts/05_combine_demographics.r")


rm(list = ls()); gc()
master_dir <- "[path_omitted]"
birth_cohorts <- 2001:2003
load("3_DESCRIPTIVE_STUDY/processed/1_cohort_spine.rda")

source("3_DESCRIPTIVE_STUDY/scripts/10_chc_diagnoses.r")
source("3_DESCRIPTIVE_STUDY/scripts/11_enrolment.r")
source("3_DESCRIPTIVE_STUDY/scripts/12_exclusion.r")
source("3_DESCRIPTIVE_STUDY/scripts/13_absence.r")
source("3_DESCRIPTIVE_STUDY/scripts/14_special_schools.r")

rm(list = ls()); gc()
master_dir <- "[path_omitted]"
birth_cohorts <- 2001:2003
files <- list.files("3_DESCRIPTIVE_STUDY/processed/")
files <- files[!(files %in% c("2_cohort_spine_complete.rda"))]
for (file in files) { load(paste0("3_DESCRIPTIVE_STUDY/processed/", file)) }
rm(file, files)

source("3_DESCRIPTIVE_STUDY/scripts/20_combine.r")
