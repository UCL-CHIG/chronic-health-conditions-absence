
print("Loading diagnoses")

# load diagnosis codes
diagnoses <- fread(paste0(master_dir, "/HES_APC_DIAG_combined.csv"),
                   header = T,
                   stringsAsFactors = F)

# Subset to those in cohort
diagnoses <- diagnoses[encrypted_hesid %in% cohort_spine$encrypted_hesid]

# get age vars
diagnoses <- merge(diagnoses,
                   cohort_spine[, c("encrypted_hesid", "inception_year", "dob_approx")],
                   by = "encrypted_hesid",
                   all.x = T)

# drop episodes before reception and beyond year 11 by cohort
diagnoses[, drop := F]
diagnoses[inception_year == 2013 & (epistart < "2005-09-01" | epistart > "2017-08-31"), drop := T]
diagnoses[inception_year == 2014 & (epistart < "2006-09-01" | epistart > "2018-08-31"), drop := T]
diagnoses[inception_year == 2015 & (epistart < "2007-09-01" | epistart > "2019-08-31"), drop := T]
diagnoses <- diagnoses[drop == F]
diagnoses[, drop := NULL]

# calculate LOS using the cleaned start and end dates
# get admission dates and startage
all_episodes <- fread(paste0(master_dir, "/HES_APC_DIS_ADMI_EPI_combined.csv"),
                      header = T,
                      stringsAsFactors = F)

all_episodes <- all_episodes[encrypted_hesid %in% diagnoses$encrypted_hesid &
                               epikey %in% diagnoses$epikey]

all_episodes[admidate >= as.Date("2020-04-01"), disdate := as.Date("2021-03-31")]
all_episodes[!is.na(epiend) & is.na(disdate), disdate := epiend]

all_episodes[, epikey := as.integer64(epikey)]

diagnoses <- merge(diagnoses,
                   all_episodes[, c("encrypted_hesid",
                                    "epikey",
                                    "admimeth",
                                    "admidate",
                                    "disdate")],
                   by = c("encrypted_hesid", "epikey"),
                   all.x = T)

rm(all_episodes); gc()

diagnoses[, admi_los_nights := as.integer(difftime(disdate, admidate, units = "days"))]
# summary(diagnoses$admi_los_nights)
# table(diagnoses$admi_los_nights < 0)

print("Getting OPCS codes")

operations <- fread(paste0(master_dir, "/HES_APC_Operations_combined.csv"),
                    header = T,
                    stringsAsFactors = F)

operations <- operations[encrypted_hesid %in% cohort_spine$encrypted_hesid]

# get age vars
operations <- merge(operations,
                    cohort_spine[, c("encrypted_hesid", "inception_year", "dob_approx")],
                    by = "encrypted_hesid",
                    all.x = T)

# drop episodes before reception and beyond year 11 by cohort
operations[, drop := F]
operations[inception_year == 2013 & (epistart < "2005-09-01" | epistart > "2017-08-31"), drop := T]
operations[inception_year == 2014 & (epistart < "2006-09-01" | epistart > "2018-08-31"), drop := T]
operations[inception_year == 2015 & (epistart < "2007-09-01" | epistart > "2019-08-31"), drop := T]
operations <- operations[drop == F]
operations[, drop := NULL]

# Hardelid ----------------------------------------------------------------

print("Hardelid codes")

hardelid <- fread("code lists/hardelid_v1.csv",
                  header = T,
                  stringsAsFactors = F)

hardelid[, code := gsub("\\.", "", code)]

diagnoses[, diag := substr(diag, 1, 4)]

diagnoses[, chc_hardelid := F]
diagnoses[, diag_for_link := diag]

diagnoses[substr(diag, 1, 3) %in% hardelid[nchar(code) == 3]$code, chc_hardelid := T]
diagnoses[substr(diag, 1, 3) %in% hardelid[nchar(code) == 3]$code, diag_for_link := substr(diag, 1, 3)]

diagnoses[substr(diag, 1, 4) %in% hardelid[nchar(code) == 4]$code, chc_hardelid := T]
diagnoses[substr(diag, 1, 4) %in% hardelid[nchar(code) == 4]$code, diag_for_link := substr(diag, 1, 4)]

hardelid_diagnoses <- diagnoses[chc_hardelid == T]
hardelid_diagnoses[, chc_hardelid := NULL]

hardelid_diagnoses <- merge(hardelid_diagnoses,
                            hardelid[, c("code", "flag", "type")],
                            by.x = "diag_for_link",
                            by.y = "code",
                            all.x = T)

rm(hardelid)

#table(hardelid_diagnoses$type, useNA = "always")

# deal with flags
hardelid_diagnoses[startage >= 7001, startage := 0]
hardelid_diagnoses[, drop := F]
hardelid_diagnoses[flag == "LOS3" & admi_los_nights < 3, drop := T]
hardelid_diagnoses[flag == "AGE10" & startage < 10, drop := T]
hardelid_diagnoses <- hardelid_diagnoses[drop == F]
hardelid_diagnoses[, drop := NULL]

# tidy
hardelid_diagnoses[, chc_group := "hardelid"]
hardelid_diagnoses <- hardelid_diagnoses[, c("encrypted_hesid", "inception_year", "diag", "type", "epistart", "chc_group")]

chc_codes <- data.table()
chc_codes <- rbind(chc_codes, hardelid_diagnoses)
rm(hardelid_diagnoses)

# Neurodis -------------------------------------------------------------------

print("Neurodis")

neurodis <- fread("code lists/neurodis/neurodisability_combined_Aug_2024.csv", stringsAsFactors = F)
neurodis <- neurodis[code != ""] # remove bwt and ga codes
neurodis <- neurodis[decision == "Y" & age_criteria == 0, c("code", "description", "code_type")]

neurodis[, code := gsub("\\.", "", code)]

diagnoses[, chc_neurodis := F]
diagnoses[substr(diag, 1, 3) %in% neurodis[code_type == "icd" & nchar(code) == 3]$code, chc_neurodis := T]
diagnoses[substr(diag, 1, 4) %in% neurodis[code_type == "icd" & nchar(code) == 4]$code, chc_neurodis := T]

neurodis_diagnoses <- diagnoses[chc_neurodis == T]
neurodis_diagnoses[, chc_neurodis := NULL]


operations[, chc_neurodis := F]
operations[substr(opertn, 1, 3) %in% neurodis[code_type == "opcs" & nchar(code) == 3]$code, chc_neurodis := T]
operations[substr(opertn, 1, 4) %in% neurodis[code_type == "opcs" & nchar(code) == 4]$code, chc_neurodis := T]
neurodis_operations <- operations[chc_neurodis == T]

# tidy
neurodis_diagnoses[, chc_group := "neurodis"]
neurodis_diagnoses[, type := "neurodis"]
neurodis_diagnoses <- neurodis_diagnoses[, c("encrypted_hesid", "inception_year", "diag", "type", "epistart", "chc_group")]
chc_codes <- rbind(chc_codes, neurodis_diagnoses)
rm(neurodis_diagnoses)

neurodis_operations[, chc_group := "neurodis"]
neurodis_operations[, type := "neurodis"]
neurodis_operations <- neurodis_operations[, c("encrypted_hesid", "inception_year", "opertn", "type", "epistart", "chc_group")]
setnames(neurodis_operations, "opertn", "diag")

chc_codes <- rbind(chc_codes, neurodis_operations)

rm(neurodis_operations)

# other_codes -------------------------------------------------------------

print("Getting other groups")

other_codes <- data.table(read_excel("3_DESCRIPTIVE_STUDY/Supplementary Table S1 - code list - version 3.xlsx",
                                     sheet = "Codes"))

other_codes <- other_codes[, c("group", "code_icd10")]

other_codes <- other_codes[!is.na(code_icd10)]
other_codes[, code_icd10 := gsub("\\.", "", code_icd10)]

asthma_exclusions <- data.table(read_excel("3_DESCRIPTIVE_STUDY/Supplementary Table S1 - code list - version 3.xlsx",
                                           sheet = "asthma_exclusions"))
asthma_exclusions <- asthma_exclusions[, c("group", "code_icd10")]
asthma_exclusions[, code_icd10 := gsub("\\.", "", code_icd10)]

srp_exclusions <- data.table(read_excel("3_DESCRIPTIVE_STUDY/Supplementary Table S1 - code list - version 3.xlsx",
                                        sheet = "srp_psych_exclusions"))
srp_exclusions <- srp_exclusions[, c("group", "code", "code_type")]
srp_exclusions[, code := gsub("\\.", "", code)]


chc_groups <- unique(other_codes$group)

for (chc in chc_groups) {
  
  print(chc)
  
  diagnoses[, tmp_chc_flag := F]
  diagnoses[substr(diag, 1, 3) %in% other_codes[group == chc & nchar(code_icd10) == 3]$code_icd10, tmp_chc_flag := T]
  diagnoses[substr(diag, 1, 4) %in% other_codes[group == chc & nchar(code_icd10) == 4]$code_icd10, tmp_chc_flag := T]
  
  if (chc == "asthma") {
    diagnoses[, tmp_asthma_check := F]
    diagnoses[substr(diag, 1, 3) %in% asthma_exclusions[nchar(code_icd10) == 3]$code_icd10, tmp_asthma_check := T]
    diagnoses[substr(diag, 1, 4) %in% asthma_exclusions[nchar(code_icd10) == 4]$code_icd10, tmp_asthma_check := T]
    diagnoses[, tmp_asthma_check := max(tmp_asthma_check), by = .(encrypted_hesid, epistart)]
    diagnoses[tmp_chc_flag == T & tmp_asthma_check == T, tmp_chc_flag := F]
    diagnoses[, tmp_asthma_check := NULL]
  }
  
  if (chc %in% c("srp_internalising", "srp_externalising", "srp_psych", "ara")) {

    diagnoses[!(admimeth %in% c(21:25, "2A", "2B", "2C", "2D", 28)), tmp_chc_flag := F]

    if (chc %in% c("srp_internalising", "srp_externalising")) {
      diagnoses[diag_no != 1, tmp_chc_flag := F]
    }
    
    if (chc == "srp_psych") {
      
      diagnoses[diag_no != 1, tmp_chc_flag := F]
      
      diagnoses[, tmp_srp_check := F]
      diagnoses[substr(diag, 1, 3) %in% srp_exclusions[nchar(code) == 3 & code_type == "icd10"]$code, tmp_srp_check := T]
      diagnoses[substr(diag, 1, 4) %in% srp_exclusions[nchar(code) == 4 & code_type == "icd10"]$code, tmp_srp_check := T]
      diagnoses[, tmp_srp_check := max(tmp_srp_check), by = .(encrypted_hesid, admidate)]
      diagnoses[tmp_chc_flag == T & tmp_srp_check == T, tmp_chc_flag := F]
      diagnoses[, tmp_srp_check := NULL]
      
      op_tmp <- operations[opertn %in% srp_exclusions[code_type == "opcs4"]$code]
      op_tmp <- op_tmp[, c("encrypted_hesid", "epistart")]
      op_tmp <- op_tmp[!duplicated(op_tmp)]
      op_tmp[, has_srp_op_excl := T]
      
      diagnoses <- merge(diagnoses,
                         op_tmp,
                         by = c("encrypted_hesid", "epistart"),
                         all.x = T)
      diagnoses[is.na(has_srp_op_excl), has_srp_op_excl := F]
      
      diagnoses[, has_srp_op_excl := max(has_srp_op_excl), by = .(encrypted_hesid, admidate)]
      diagnoses[tmp_chc_flag == T & has_srp_op_excl == T, tmp_chc_flag := F]
      diagnoses[, has_srp_op_excl := NULL]
      
    }
    
  }
  
  tmp_diagnoses <- diagnoses[tmp_chc_flag == T]
  tmp_diagnoses[, tmp_chc_flag := NULL]
  
  tmp_diagnoses[, chc_group := chc]
  tmp_diagnoses[, type := chc]
  tmp_diagnoses <- tmp_diagnoses[, c("encrypted_hesid", "inception_year", "diag", "type", "epistart", "chc_group")]
  
  
  chc_codes <- rbind(chc_codes, tmp_diagnoses)
  chc_codes <- chc_codes[!duplicated(chc_codes)]
  
}

rm(chc, chc_groups, tmp_diagnoses)

# cleft lip --------------------------------------------------------------

print("Cleft lip and palate operation codes")

# codes F031 (cleft lip) and F291 (cleft palate)

cleft_lip_op <- operations[opertn == "F031", c("encrypted_hesid", "inception_year", "opertn", "epistart")]
cleft_pal_op <- operations[opertn == "F291", c("encrypted_hesid", "inception_year","opertn", "epistart")]

cleft_lip_op[, chc_group := "cleft_lip"]
cleft_lip_op[, type := "cleft_lip"]

cleft_pal_op[, chc_group := "cleft_palate"]
cleft_pal_op[, type := "cleft_palate"]

setnames(cleft_lip_op, "opertn", "diag")
setnames(cleft_pal_op, "opertn", "diag")

ord <- names(chc_codes)
cleft_lip_op <- cleft_lip_op[, ..ord]
cleft_pal_op <- cleft_pal_op[, ..ord]

chc_codes <- rbind(chc_codes, cleft_lip_op, cleft_pal_op)

rm(cleft_lip_op, cleft_pal_op)

# Save --------------------------------------------------------------------

print("Saving")

save(chc_codes, file = "3_DESCRIPTIVE_STUDY/processed/chc_codes.rda")

rm(chc_codes, ord, operations); gc()
