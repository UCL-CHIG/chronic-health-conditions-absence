# load

years <- 2013:2019 # to cover years 7 to 11
print("Loading exclusion data")

# main censuses
dbhandle <- odbcDriverConnect('[conn_str_omitted]')
tables <- subset(sqlTables(dbhandle), TABLE_SCHEM == "dbo")

keep <- tables$TABLE_NAME[grepl("Exclusions", tables$TABLE_NAME)]
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
    cat_column <- grepl("category", temp_columns_lower)
    sessions_column <- grepl("sessions", temp_columns_lower)
    startdate_column <- grepl("startdate", temp_columns_lower)
    
    pupil_column <- temp_columns[pupil_column]
    year_column <- temp_columns[year_column]
    cat_column <- temp_columns[cat_column]
    sessions_column <- temp_columns[sessions_column]
    startdate_column <- temp_columns[startdate_column]
    
    if (identical(pupil_column, character(0)) == FALSE & identical(year_column, character(0)) == FALSE & identical(cat_column, character(0)) == FALSE & identical(sessions_column, character(0)) == FALSE & identical(startdate_column, character(0)) == FALSE) {
      temp <- data.table(sqlQuery(dbhandle, paste0("SELECT ", pupil_column , ", ", year_column, ", ", cat_column, ", ", sessions_column, ", ", startdate_column, " FROM ", table_name)))
      temp <- temp[get(pupil_column) %in% cohort_spine$PupilMatchingRefAnonymous]
      
      if (ncol(temp) == 5) {
        colnames(temp) <- c("PupilMatchingRefAnonymous", "AcademicYear", "exclsuion_category", "sessions", "startdate")
        npd_source <- rbind(npd_source, temp)
      }
    }
    
  }
  
  return(npd_source)
}

exclusions <- generate_npd_source()
exclusions <- exclusions[!duplicated(exclusions)]

exclusions[, AcademicYear := as.integer(substr(AcademicYear, 6, 9))]

print("Subsetting")
exclusions <- merge(exclusions,
                    cohort_spine[, c("PupilMatchingRefAnonymous", "inception_year")],
                    by = "PupilMatchingRefAnonymous",
                    all.x = T)

exclusions[inception_year == 2013, year := AcademicYear - 2006]
exclusions[inception_year == 2014, year := AcademicYear - 2007]
exclusions[inception_year == 2015, year := AcademicYear - 2008]

exclusions <- exclusions[year >= 7 & year <= 11]

print("Saving")
save(exclusions, file = "3_DESCRIPTIVE_STUDY/PROCESSED/exclusions.rda")
rm(exclusions); gc()