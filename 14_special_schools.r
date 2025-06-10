
years <- 2013:2019 # to cover years 7 to 11
print("Getting URN and special school enrolments")

# main censuses
dbhandle <- odbcDriverConnect('[conn_str_omitted]')
tables <- subset(sqlTables(dbhandle), TABLE_SCHEM == "dbo")

keep <- tables$TABLE_NAME[grepl("Autumn|Spring|Summer", tables$TABLE_NAME)]
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
    year_column <- grepl("^academicyear", temp_columns_lower)
    urn_column <- grepl("urn", temp_columns_lower)

    pupil_column <- temp_columns[pupil_column]
    year_column <- temp_columns[year_column]
    urn_column <- temp_columns[urn_column]
    
    temp <- data.table(sqlQuery(dbhandle, paste0("SELECT ",
                                                 pupil_column , ", ",
                                                 year_column, ", ",
                                                 urn_column,
                                                 " FROM ", table_name)))
    
    temp <- temp[get(pupil_column) %in% cohort_spine$PupilMatchingRefAnonymous]
    colnames(temp) <- c("PupilMatchingRefAnonymous", "AcademicYear", "urn")
    npd_source <- rbind(npd_source, temp)
    
  }
  
  return(npd_source)
}

print("Main censuses")

urn_dt <- generate_npd_source()

urn_dt[, AcademicYear := as.integer(substr(AcademicYear, 6, 9))]

urn_dt <- merge(urn_dt,
                cohort_spine[, c("PupilMatchingRefAnonymous", "inception_year")],
                by = "PupilMatchingRefAnonymous",
                all.x = T)

urn_dt[inception_year == 2013, year := AcademicYear - 2006]
urn_dt[inception_year == 2014, year := AcademicYear - 2007]
urn_dt[inception_year == 2015, year := AcademicYear - 2008]

urn_dt <- urn_dt[year >= 7 & year <= 11]

# PRU
print("PRU")
pru <- data.table(sqlQuery(dbhandle,
                           "SELECT PRU_PupilMatchingRefAnonymous, PRU_AcademicYear, PRU_URN FROM PRU_Census_2010_to_2013 WHERE PRU_AcademicYear = '2012/2013'"
                           )
                  )

setnames(pru, c("PupilMatchingRefAnonymous", "AcademicYear", "urn"))
pru <- pru[pru$PupilMatchingRefAnonymous %in% cohort_spine$PupilMatchingRefAnonymous]

pru[, AcademicYear := as.integer(substr(AcademicYear, 6, 9))]

pru <- merge(pru,
             cohort_spine[, c("PupilMatchingRefAnonymous", "inception_year")],
             by = "PupilMatchingRefAnonymous",
             all.x = T)

pru[inception_year == 2013, year := AcademicYear - 2006]
pru[inception_year == 2014, year := AcademicYear - 2007]
pru[inception_year == 2015, year := AcademicYear - 2008]

pru <- pru[year >= 7 & year <= 11]

# AP
print("AP")
ap <- data.table(sqlQuery(dbhandle,
                          "SELECT AP_PupilMatchingRefAnonymous, AP_ACADYR FROM AP_Census_2008_to_2020")
)

setnames(ap, c("PupilMatchingRefAnonymous", "AcademicYear"))
ap <- ap[ap$PupilMatchingRefAnonymous %in% cohort_spine$PupilMatchingRefAnonymous]
ap[, AcademicYear := as.integer(substr(AcademicYear, 6, 9))]
ap[, urn := -99]

ap <- merge(ap,
            cohort_spine[, c("PupilMatchingRefAnonymous", "inception_year")],
            by = "PupilMatchingRefAnonymous",
            all.x = T)

ap[inception_year == 2013, year := AcademicYear - 2006]
ap[inception_year == 2014, year := AcademicYear - 2007]
ap[inception_year == 2015, year := AcademicYear - 2008]

ap <- ap[year >= 7 & year <= 11]

# Combine
print("Combine")
urn_dt <- rbind(urn_dt, pru)
urn_dt <- rbind(urn_dt, ap)
rm(pru, ap)

# Get special schools
print("Identify special schools")
edubase <- fread("[file_path_suppressed]/public_data_gias_data_25102021_20220614.csv")
edubase <- edubase[, c("URN", "TypeOfEstablishment (name)", "EstablishmentTypeGroup (name)")]
setnames(edubase, names(edubase), c("urn", "typeofest", "estgroup"))

edubase[, typeofest := tolower(typeofest)]
edubase[, estgroup := tolower(estgroup)]

edubase <- edubase[grepl("special", typeofest) | grepl("special", estgroup)]
edubase[, special_school := T]

urn_dt <- merge(urn_dt,
                edubase[, c("urn", "special_school")],
                by = "urn",
                all.x = T)

print("Subset and save")
special_schools <- urn_dt[special_school == T | urn == -99]
special_schools[, special_school := NULL]

special_schools <- special_schools[order(PupilMatchingRefAnonymous, year),
                                   c("PupilMatchingRefAnonymous", "year", "urn")]

special_schools <- special_schools[!duplicated(special_schools)]

save(special_schools, file = "3_DESCRIPTIVE_STUDY/processed/special_schools.rda")

rm(urn_dt, edubase, dbhandle, tables, generate_npd_source, years)
