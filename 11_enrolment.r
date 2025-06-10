
# load
years <- 2014:2019 # to cover years 8 to 11

print("Loading enrolment data")

# main censuses
dbhandle <- odbcDriverConnect('[conn_str_omitted]')
tables <- subset(sqlTables(dbhandle), TABLE_SCHEM == "dbo")

keep <- tables$TABLE_NAME[grepl("Spring|Summer|Autumn", tables$TABLE_NAME)]
tables <- tables[tables$TABLE_NAME %in% keep, ]
keep <- tables$TABLE_NAME[grepl(paste0(years, collapse = "|"), tables$TABLE_NAME)]
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
    year_column <- grepl("^academicyear", temp_columns_lower) # ^ = starting with as there are some other cols with AcademicYear in the name
    
    pupil_column <- temp_columns[pupil_column]
    year_column <- temp_columns[year_column]
    
    if (identical(pupil_column, character(0)) == FALSE & identical(year_column, character(0)) == FALSE) {
      temp <- data.table(sqlQuery(dbhandle, paste0("SELECT ", pupil_column , ", ", year_column, " FROM ", table_name)))
      temp <- temp[get(pupil_column) %in% cohort_spine$PupilMatchingRefAnonymous]
      
      if (ncol(temp) == 2) {
        colnames(temp) <- c("PupilMatchingRefAnonymous", "AcademicYear")
        temp <- distinct(temp)
        temp <- subset(temp, !is.na(AcademicYear))
        temp$census <- table_name
        npd_source <- rbind(npd_source, temp)
      }
    }
    
  }
  
  return(npd_source)
}

enrolments <- generate_npd_source()

enrolments[, AcademicYear := as.integer(substr(AcademicYear, 6, 9))]
enrolments[, census := gsub("_.*", "", census)]

enrolments <- merge(enrolments,
                    cohort_spine[, c("PupilMatchingRefAnonymous", "inception_year")],
                    by = "PupilMatchingRefAnonymous",
                    all.x = T)

enrolments[inception_year == 2013, year := AcademicYear - 2006]
enrolments[inception_year == 2014, year := AcademicYear - 2007]
enrolments[inception_year == 2015, year := AcademicYear - 2008]

# drop anything other than years 8 to 11
enrolments <- enrolments[year >= 8 & year <= 11]

# AP census
print("Adding AP")
temp <- data.table(sqlQuery(dbhandle, paste0("SELECT AP_PupilMatchingRefAnonymous,
                                             AP_ACADYR
                                             FROM AP_Census_2008_to_2020
                                             WHERE AP_ACADYR IN
                                             ('2013/2014',
                                             '2014/2015',
                                             '2015/2016',
                                             '2016/2017',
                                             '2017/2018',
                                             '2018/2019')")))

temp[, AcademicYear := as.integer(substr(AP_ACADYR, 6, 9))]
temp[, census := "AP"]

temp <- temp[, c("AP_PupilMatchingRefAnonymous", "AcademicYear", "census")]
setnames(temp, "AP_PupilMatchingRefAnonymous", "PupilMatchingRefAnonymous")

temp <- temp[PupilMatchingRefAnonymous %in% cohort_spine$PupilMatchingRefAnonymous]

temp <- distinct(temp)
temp <- subset(temp, !is.na(AcademicYear))

print("Subsetting")
temp <- merge(temp,
              cohort_spine[, c("PupilMatchingRefAnonymous", "inception_year")],
              by = "PupilMatchingRefAnonymous",
              all.x = T)

temp[inception_year == 2013, year := AcademicYear - 2006]
temp[inception_year == 2014, year := AcademicYear - 2007]
temp[inception_year == 2015, year := AcademicYear - 2008]

temp <- temp[year >= 8 & year <= 11]
table(temp$year)

enrolments <- rbind(enrolments, temp)
rm(temp)

enrolments <- enrolments[order(PupilMatchingRefAnonymous, year, census)]

print("Saving")
save(enrolments, file = "3_DESCRIPTIVE_STUDY/processed/enrolments.rda")
rm(enrolments); gc()