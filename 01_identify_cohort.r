
# *******************************************
# Name: identify cohort
# Created by: Matthew Jay
# *******************************************

# GET YEAR 7 ENROLMENT ----------------------------------------------------

year7_cohorts <- birth_cohorts + 12

print(paste0("Getting year 7 enrolment for birth cohorts ", paste(birth_cohorts, collapse = ", "),
      "; year 7 in ", paste(year7_cohorts, collapse = ", ")))

dbhandle <- odbcDriverConnect('[omitted]')

tables <- subset(sqlTables(dbhandle), TABLE_SCHEM == "dbo")

keep <- tables$TABLE_NAME[grepl("Autumn|Spring|Summer", tables$TABLE_NAME)]
tables <- tables[tables$TABLE_NAME %in% keep, ]

keep <- tables$TABLE_NAME[grepl(paste0(year7_cohorts, collapse = "|"), tables$TABLE_NAME)]
tables <- tables[tables$TABLE_NAME %in% keep, ]
rm(keep)

generate_npd_source <- function() {
  npd_source <- data.table()
  
  for(table_name in unique(tables$TABLE_NAME)) {
    
    gc()
    print(paste0("Now doing table: ", table_name))
    
    temp <- sqlQuery(dbhandle, paste0("SELECT * FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME =  '", table_name, "'"))
    temp_columns <- temp$COLUMN_NAME
    temp_columns_lower <- tolower(temp_columns)
    
    pupil_column <- grepl("pupilmatchingrefanonymous", temp_columns_lower)
    age_column <- grepl("^ageatstartofacademicyear", temp_columns_lower)
    ncyearactualcolumn <- grepl("ncyearactual", temp_columns_lower)
    
    pupil_column <- temp_columns[pupil_column]
    age_column <- temp_columns[age_column]
    ncyearactualcolumn <- temp_columns[ncyearactualcolumn]
    
    temp <- data.table(sqlQuery(dbhandle, paste0("SELECT ",
                                                 pupil_column , ", ",
                                                 age_column, ", ",
                                                 ncyearactualcolumn, " FROM ", table_name, 
                                                 " WHERE ", ncyearactualcolumn, " = '7' OR ",
                                                 " (", ncyearactualcolumn, " = 'X' AND ", age_column, " = '11')")))
    
    temp[, census := tolower(table_name)]
    temp[, census_year := gsub("[a-z]*_census_", "", census)]
    temp[, census_term := gsub("_census_[0-9].*", "", census)]
    temp[, census := NULL]
    
    if (ncol(temp) == 5) {
      colnames(temp) <- c("PupilMatchingRefAnonymous", "ageatstartofacademicyear", "ncyearactual", "census_year", "census_term")
      npd_source <- rbind(npd_source, temp)
    }
    
  }
  
  return(npd_source)
}

cohort_spine <- generate_npd_source()

cohort_spine <- cohort_spine[!duplicated(cohort_spine[, c("PupilMatchingRefAnonymous", "census_year")])]
length(unique(cohort_spine$PupilMatchingRefAnonymous)); nrow(cohort_spine)

# there are some children held back in year 7. We will take the first one.
cohort_spine[, n_records := seq_len(.N), by = PupilMatchingRefAnonymous]
cohort_spine <- cohort_spine[n_records == 1]
length(unique(cohort_spine$PupilMatchingRefAnonymous)); nrow(cohort_spine)

cohort_spine[, census_term := NULL]
cohort_spine[, n_records := NULL]
cohort_spine[, ageatstartofacademicyear := NULL]
cohort_spine[, ncyearactual := NULL]

# add in AP
ap <- data.table(sqlQuery(dbhandle,
               paste0(
                "SELECT AP_PupilMatchingRefAnonymous, AP_ACADYR FROM AP_Census_2008_to_2020
                 WHERE AP_ACADYR IN ('2012/2013', '2013/2014', '2014/2015')
                 AND AP_Age_Start = 11")))

setnames(ap, c("PupilMatchingRefAnonymous", "census_year"))
ap[, census_year := as.integer(substr(census_year, 6, 9))]
ap <- ap[!(ap$PupilMatchingRefAnonymous %in% cohort_spine$PupilMatchingRefAnonymous)]
length(unique(ap$PupilMatchingRefAnonymous)); nrow(ap)
ap <- ap[!duplicated((ap$PupilMatchingRefAnonymous))]

cohort_spine <- rbind(cohort_spine, ap)
cohort_spine <- cohort_spine[order(PupilMatchingRefAnonymous, census_year)]
length(unique(cohort_spine$PupilMatchingRefAnonymous)); nrow(cohort_spine)

# add pru for 2013 (last year of PRU census)
pru <- data.table(sqlQuery(dbhandle,
                          paste0(
                            "SELECT [PRU_PupilMatchingRefAnonymous]
      ,[PRU_AcademicYear]
  FROM [dbo].[PRU_Census_2010_to_2013]
  WHERE [PRU_AcademicYear] = '2012/2013'
  AND ([PRU_NCyearActual] = '7' OR ([PRU_NCyearActual] = 'X' AND [PRU_AgeAtStartOfAcademicYear] = 11))")))

setnames(pru, c("PupilMatchingRefAnonymous", "census_year"))

pru[, census_year := as.integer(substr(census_year, 6, 9))]
pru <- pru[!(pru$PupilMatchingRefAnonymous %in% cohort_spine$PupilMatchingRefAnonymous)]
length(unique(pru$PupilMatchingRefAnonymous)); nrow(pru)

cohort_spine <- rbind(cohort_spine, pru)
cohort_spine <- cohort_spine[order(PupilMatchingRefAnonymous, census_year)]
length(unique(cohort_spine$PupilMatchingRefAnonymous)); nrow(cohort_spine)

rm(tables, ap, pru); gc()

# GET KS1 ENROLMENTS ------------------------------------------------------

generate_npd_source <- function(ncyearval, ageval) {
  npd_source <- data.table()
  
  for(table_name in unique(tables$TABLE_NAME)) {
    
    gc()
    print(paste0("Now doing table: ", table_name))
    
    temp <- sqlQuery(dbhandle, paste0("SELECT * FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME =  '", table_name, "'"))
    temp_columns <- temp$COLUMN_NAME
    temp_columns_lower <- tolower(temp_columns)
    
    pupil_column <- grepl("pupilmatchingrefanonymous", temp_columns_lower)
    age_column <- grepl("^ageatstartofacademicyear", temp_columns_lower)
    ncyearactualcolumn <- grepl("ncyearactual", temp_columns_lower)
    
    pupil_column <- temp_columns[pupil_column]
    age_column <- temp_columns[age_column]
    ncyearactualcolumn <- temp_columns[ncyearactualcolumn]
    
    temp <- data.table(sqlQuery(dbhandle, paste0("SELECT ",
                                                 pupil_column,# ", ",
                                                 #age_column, ", ",
                                                 #ncyearactualcolumn,
                                                 " FROM ", table_name, 
                                                 " WHERE ", ncyearactualcolumn, " = '", ncyearval, "' OR ",
                                                 " (", ncyearactualcolumn, " = 'X' AND ", age_column, " = '", ageval,"')")))
    
    # temp[, census := tolower(table_name)]
    # temp[, census_year := gsub("[a-z]*_census_", "", census)]
    # temp[, census_term := gsub("_census_[0-9].*", "", census)]
    # temp[, census := NULL]
    
    #if (ncol(temp) == 5) {
      #colnames(temp) <- c("PupilMatchingRefAnonymous", "ageatstartofacademicyear", "ncyearactual", "census_year", "census_term")
      colnames(temp) <- c("PupilMatchingRefAnonymous")
      npd_source <- rbind(npd_source, temp)
    #}
    
  }
  
  return(npd_source)
}

print(paste0("Getting reception enrolments"))
cohorts <- birth_cohorts + 5

tables <- subset(sqlTables(dbhandle), TABLE_SCHEM == "dbo")
keep <- tables$TABLE_NAME[grepl("Autumn|Spring|Summer", tables$TABLE_NAME)]
tables <- tables[tables$TABLE_NAME %in% keep, ]
keep <- tables$TABLE_NAME[grepl(paste0(cohorts, collapse = "|"), tables$TABLE_NAME)]
tables <- tables[tables$TABLE_NAME %in% keep, ]
rm(keep)

reception <- generate_npd_source("R", 4)

ap <- data.table(sqlQuery(dbhandle,
                          paste0(
                            "SELECT AP_PupilMatchingRefAnonymous FROM AP_Census_2008_to_2020
                 WHERE AP_ACADYR IN ('2005/2006', '2006/2007', '2007/2008')
                 AND AP_Age_Start = 4")))


setnames(ap, c("PupilMatchingRefAnonymous"))
reception <- rbind(reception, ap)
rm(ap)

print(paste0("Getting year 1 enrolments"))
cohorts <- birth_cohorts + 6

tables <- subset(sqlTables(dbhandle), TABLE_SCHEM == "dbo")
keep <- tables$TABLE_NAME[grepl("Autumn|Spring|Summer", tables$TABLE_NAME)]
tables <- tables[tables$TABLE_NAME %in% keep, ]
keep <- tables$TABLE_NAME[grepl(paste0(cohorts, collapse = "|"), tables$TABLE_NAME)]
tables <- tables[tables$TABLE_NAME %in% keep, ]
rm(keep)

year1 <- generate_npd_source("1", 5)

ap <- data.table(sqlQuery(dbhandle,
                          paste0(
                            "SELECT AP_PupilMatchingRefAnonymous FROM AP_Census_2008_to_2020
                 WHERE AP_ACADYR IN ('2006/2007', '2007/2008', '2008/2009')
                 AND AP_Age_Start = 5")))

setnames(ap, c("PupilMatchingRefAnonymous"))
year1 <- rbind(year1, ap)
rm(ap)

print(paste0("Getting year 2 enrolments"))
cohorts <- birth_cohorts + 7

tables <- subset(sqlTables(dbhandle), TABLE_SCHEM == "dbo")
keep <- tables$TABLE_NAME[grepl("Autumn|Spring|Summer", tables$TABLE_NAME)]
tables <- tables[tables$TABLE_NAME %in% keep, ]
keep <- tables$TABLE_NAME[grepl(paste0(cohorts, collapse = "|"), tables$TABLE_NAME)]
tables <- tables[tables$TABLE_NAME %in% keep, ]
rm(keep)

year2 <- generate_npd_source("2", 6)

ap <- data.table(sqlQuery(dbhandle,
                          paste0(
                            "SELECT AP_PupilMatchingRefAnonymous FROM AP_Census_2008_to_2020
                 WHERE AP_ACADYR IN ('2007/2008', '2008/2009', '2009/2010')
                 AND AP_Age_Start = 6")))

setnames(ap, c("PupilMatchingRefAnonymous"))
year2 <- rbind(year2, ap)
rm(ap)


print("Creating KS1 enrolment flag")

cohort_spine[, enrolled_r := PupilMatchingRefAnonymous %in% reception$PupilMatchingRefAnonymous]
cohort_spine[, enrolled_year1 := PupilMatchingRefAnonymous %in% year1$PupilMatchingRefAnonymous]
cohort_spine[, enrolled_year2 := PupilMatchingRefAnonymous %in% year2$PupilMatchingRefAnonymous]

table(cohort_spine$census_year, cohort_spine$enrolled_r)
table(cohort_spine$census_year, cohort_spine$enrolled_year1)
table(cohort_spine$census_year, cohort_spine$enrolled_year2)

cohort_spine[, enrolled_ks1 := enrolled_r | enrolled_year1 | enrolled_year2]
table(cohort_spine$census_year, cohort_spine$enrolled_ks1)

cohort_spine <- cohort_spine[, c("PupilMatchingRefAnonymous", "census_year", "enrolled_ks1")]
setnames(cohort_spine, "census_year", "inception_year")

rm(cohorts, tables, reception, year1, year2)

# GET LINKAGE KEY ---------------------------------------------------------------

print("Adding linkage key")

linkage_spine <- fread(paste0(master_dir, "/NPD_HES_Linkage_Spine.csv"),
                       header = T,
                       stringsAsFactors = F)

linkage_spine <- linkage_spine[PupilMatchingRefAnonymous %in% cohort_spine$PupilMatchingRefAnonymous]

length(unique(linkage_spine$PupilMatchingRefAnonymous)); nrow(linkage_spine)

cohort_spine <- merge(cohort_spine,
                      linkage_spine[, c("PupilMatchingRefAnonymous", "encrypted_hesid")],
                      by = "PupilMatchingRefAnonymous",
                      all.x = T)

rm(linkage_spine); gc()
