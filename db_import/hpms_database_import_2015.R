# hpms_database_import.R

# 12 Apr 2017
# Matt Landis

# HPMS 2015 data sent to us from justin.clarke@dot.gov as five zipped CSV files.
# This script combines them into a single file for import into a database.


# Initialize ===============================================================
library('RODBC')
library('tidyverse')
library('data.table')
library('stringr')
#library('dbplyr') # installed from github
library('readxl')

#q_drive <- '//i-rsg.com/rsgshares/Projects/_Federal/FHWA/15__ HPMS_DataVisualizationSupport'
onedrive <- file.path('C:/Users/matt.landis/OneDrive - Resource Systems Group, Inc')
proj_dir <- file.path(onedrive, 'Projects', 'Fhwa_hpms')
d_dir <- file.path(proj_dir, 'data')

local_con <- 'Driver={SQL Server Native Client 11.0}; server=10548L\\SQLEXPRESS;database=HPMS2016;trusted_connection=yes;'


# Work ======================================================================


# Load 2015 Sections data --------------------------------------------------

# Push the flat files for sections into a database

choice <- menu(choices = c('Replace it', 'Stop'),
               title='Replacing "Review_Sections" table takes ~ 4 hrs!!')

if ( choice == 1 ){

  raw_dir <- file.path(proj_dir, 'raw_data_2015')
  
  files <- Sys.glob(file.path(raw_dir, '*/*.csv'))
  
  # Create connection to database
  
  # Prepare to write to the database
  tablename <- 'Review_sections_staging'
  var_types <- c(DataYear='smallint',
                 StateId='smallint',
                 RouteId='varchar(120)',
                 BeginPoint='decimal(8,3)',
                 EndPoint='decimal(8,3)',
                 SectionLength='decimal(8,3)',
                 DataItem='varchar(25)',
                 ValueNumeric='decimal(10,3)',
                 ValueText='varchar(50)',
                 ValueDate='datetime',
                 StateYearKey='integer')
  
  
  con <- odbcDriverConnect(connection=local_con)
  
  for ( i in seq_along(files) ){
    
    out_tbl <- read_delim(files[i], delim='|', quote="",
                          col_types=cols(.default=col_character()))
    
    # Rename columns to match schema
    
    out_tbl1 <- out_tbl %>%
      rename(DataYear = Year_record,
             RouteId = route_ID,
             BeginPoint = begin_Point,
             EndPoint = endpoint,
             DataItem = dataitem) %>%
      mutate(SectionLength = as.numeric(EndPoint) - as.numeric(BeginPoint),
             StateYearKey = str_c(StateId, str_sub(DataYear, start = 3, end=4)))
    
    ## # Test writing the table.
    ## sqlDrop(con, sqtable = tablename)
    ## sqlSave(con, head(out_tbl1, 10), tablename=tablename,
    ##         append=FALSE, rownames=FALSE, verbose=TRUE, nastring=NULL,
    ##         varTypes = var_types)
    
    ## test <- sqlFetch(con, tablename)
    ## test 
    
    if ( i == 1 ){
      # Actually write the table.
      sqlDrop(con, sqtable=tablename)
      sqlSave(con, out_tbl1, tablename=tablename, verbose=FALSE, nastring=NULL,
              varTypes=var_types, fast=TRUE, rownames=FALSE)
    } else {
      sqlSave(con, out_tbl1, tablename=tablename, verbose=FALSE, nastring=NULL,
              varTypes=var_types, fast=TRUE, rownames=FALSE, append=TRUE)
    }
    
    cat('\n\nWrote table ', i, '\n\n')
    
  } 
  
  close(con)
}

# Load 2014 Sections data --------------------------------------------------

# Copied from Q:\Projects\_Federal\FHWA\15__ HPMS_DataVisualizationSupport\HPMS Files

choice <- menu(choices = c('Replace it', 'Stop'),
               title='Replacing "Review_Sections_2014" table takes ~3 hrs!!')

if ( choice == 1 ){

  raw_dir <- file.path(proj_dir, 'raw_data_2014')

  files <- Sys.glob(file.path(raw_dir, '*/*.csv'))

  # files[1:3] are 2015 data!
  #(lns <- read_lines(files[4]))

  # Note how messed up this file is.  Variable number of fields!

  df <- read.table(files[4], header=TRUE, sep='|', quote='', as.is=TRUE,
                   colClasses='character', fill=TRUE, skipNul=TRUE)

  #length(lns) - nrow(dt)           

  # Checks
  sort(unique(df$DataItem))
  r <- which(df$DataItem == '2.670')
  df[r:(r + 10), ]

  # Replace row 19415493 -- it has an extra column
  df[r, ] <- c(as.character(df[r,])[-3], NA)

  # Examine values of each column
  sort(unique(df$DataItem))
  (r <- which(df$DataItem == ''))
  df[r, ]

  (r <- which(is.na(df$DataItem)))

  unique(df$DataYear)
  unique(df$StateId)  # Only has 40 states.  
  range(as.numeric(df$BeginPoint), na.rm=TRUE)
  range(as.numeric(df$EndPoint), na.rm=TRUE)
  range(as.numeric(df$ValueNumeric), na.rm=TRUE)
  df$ValueNumeric[is.na(as.numeric(df$ValueNumeric)) & !is.na(df$ValueNumeric) &
                     df$ValueNumeric != '']
  range(df$ValueText, na.rm=TRUE)
  range(df$ValueDate, na.rm=TRUE)

  # Prepare to write to the database

  dt <- data.table(df); rm(df); gc()
  dt <- dt[!duplicated(dt)]
  
  dt[, SectionLength := as.numeric(EndPoint) - as.numeric(BeginPoint)]
  dt[, StateYearKey := str_c(StateId, str_sub(DataYear, start=3, end=4))]

  count(dt, is.na(RouteId))
  count(dt, is.na(BeginPoint))
  count(dt, is.na(EndPoint))
  count(dt, is.na(DataItem))
  count(dt, is.na(StateYearKey))

  # Convert empty strings to NA
  count(dt, DataYear == '')
  count(dt, StateId == '')
  count(dt, RouteId == '')

  dt[RouteId == '']
  dt <- dt[RouteId != '']

  count(dt, BeginPoint == '')
  #dt[BeginPoint == ''] %>% View()
  dt <- dt[BeginPoint != '']

  count(dt, EndPoint == '')
  count(dt, DataItem == '')

  dt[ValueNumeric == '', ValueNumeric := NA]
  dt[ValueText == '', ValueText := NA]
  dt[ValueDate == '', ValueDate := NA]

  tablename <- 'Review_Sections_2014'
  var_types <- c(DataYear='smallint',
                 StateId='smallint',
                 RouteId='varchar(120)',
                 BeginPoint='decimal(8,3)',
                 EndPoint='decimal(8,3)',
                 SectionLength='decimal(8,3)',
                 DataItem='varchar(25)',
                 ValueNumeric='decimal(10,3)',
                 ValueText='varchar(50)',
                 ValueDate='datetime',
                 StateYearKey='integer')

  con <- odbcDriverConnect(connection=local_con)

  # Save to the table
  sqlDrop(con, sqtable=tablename)
  sqlSave(con, dt, tablename=tablename, verbose=FALSE, nastring=NULL,
          varTypes=var_types, fast=TRUE, rownames=FALSE)

  close(con)
}


# Create 2014, 2015 samples table ===================================================

# The samples table needs to be created from two tables in the HPMS database
# on wrjsqlvdw02.

choice <- menu(choices = c('Replace it', 'Stop'), title='Replacing Review_Sample_Sections!!')

if ( choice == 1 ){
  wrj_con <- 'Driver={SQL Server Native Client 11.0}; server=wrjsqlvdw02;database=HPMS;trusted_connection=yes;'
  
  con <- odbcDriverConnect(connection=wrj_con)
  
  sm_2014 <- sqlFetch(con, 'samples2014', stringsAsFactors=FALSE) %>%
    as_tibble() %>%
    rename(DataYear = YEAR_RECORD,
           StateId = STATE_CODE,
           RouteId = ROUTE_ID,
           BeginPoint = BEGIN_POINT,
           EndPoint = END_POINT,
           SampleId = SAMPLE_ID,
           ExpansionFactor = EXPANSION_FACTOR) %>%
    mutate(SectionLength = EndPoint - BeginPoint,
           StateYearKey = str_c(StateId, str_sub(DataYear, start=3, end=4)))
  
  # Check for duplicate rows
  nrow(sm_2014)
  nrow(distinct(sm_2014))
  
  # Drop duplicates
  sm_2014 <- distinct(sm_2014)
  
  # Drop near duplicates
  sm_2014 <- sm_2014 %>%
    group_by(RouteId, BeginPoint, EndPoint, StateYearKey) %>%
    mutate(drop = seq(from=0, to=(n() - 1))) %>%
    ungroup() %>%
    filter(drop == 0) %>%
    select(-drop)
  
  sm_2015 <- sqlFetch(con, 'Samples2015',
                      stringsAsFactors=FALSE) %>%
    as_tibble() %>%
    mutate(SectionLength = EndPoint - BeginPoint,
           StateYearKey = str_c(StateId, str_sub(DataYear, start=3, end=4)))
  
  nrow(sm_2015)
  nrow(distinct(sm_2015))
  
  sm_tbl <- bind_rows(sm_2014, sm_2015)
  
  close(con)
  
  var_types <- c(DataYear='smallint',
                 StateId='smallint',
                 RouteId='varchar(120)',
                 BeginPoint='decimal(8,3)',
                 EndPoint='decimal(8,3)',
                 SectionLength='decimal(8,3)',
                 SampleId='varchar(12)',
                 ExpansionFactor='decimal(8,3)',
                 Invalid='smallint',
                 StateYearKey='integer')
  
  con <- odbcDriverConnect(connection=local_con)
  sm_tbl_name <- 'Review_Sample_Sections_staging'
  sqlDrop(con, sqtable=sm_tbl_name)
  sqlSave(con, dat=sm_tbl, tablename=sm_tbl_name, verbose=FALSE, nastring=NULL,
          varTypes=var_types, fast=TRUE, rownames=FALSE)
  
}

#test <- sqlFetch(con, '2015SubmissionDates')
#head(test)


# Add summary table for data checking =====================================

# Table sent to Kevin from Justin by e-mail on 6 Apr 2017 as
# 2015HPMS_SectionSummaries.xlsx

# Load summary table.

sum_file <- file.path(raw_dir, '2015HPMS_SectionSummaries.xlsx')

sum_tbl <- read_excel(sum_file) %>%
  rename(StateId = state_Code,
         DataItem = data_Item,
         Miles = miles,
         RouteId_Count = route_ID_Count) %>%
  mutate(DataYear = '2015',
         StateYearKey = str_c(StateId, as.numeric(DataYear) %% 100))
  

var_types <- c(DataYear = 'smallint',
               StateId = 'smallint',
               DataItem = 'varchar(25)',
               Record_Count = 'integer',
               Miles = 'decimal(10,3)',
               RouteId_Count = 'integer',
               StateYearKey='integer')

names(sum_tbl) %in% names(var_types)
names(var_types) %in% names(sum_tbl)

con <- odbcDriverConnect(connection=local_con)
sum_tbl_name <- 'Section_Summaries'
sqlDrop(con, sqtable=sum_tbl_name)
sqlSave(con, dat=sum_tbl, tablename=sum_tbl_name, verbose=FALSE, nastring=NULL,
        varTypes=var_types, fast=TRUE, rownames=FALSE)
close(con)

# In SSMS, I made a primary key out of DataYear, StateId, DataItem, StateYearKey





### unused ---------------------------------------------------------------

# Create primary keys on tables ===========================================

# Did this directly in SSMS. Some code for debugging duplicate keys follows.


# samples table: RouteId, BeginPoint, EndPoint, StateYearKey
# sections table: RouteId, BeginPoint, EndPoint, DataItem, StateYearKey

# Identify duplicate primary keys in samples table.
# tab <- count(sm_2014, RouteId, BeginPoint, EndPoint, StateYearKey) %>%
#   filter(n > 1)
# tab
# 
# jn <- inner_join(tab, sm_tbl)
# jn
# 
# 
# ex <- tab$RouteId[1]
# filter(sm_tbl,
#        RouteId == ex,
#        BeginPoint == 0.779,
#        EndPoint == 0.786,
#        StateYearKey == 414) %>% print(n=150)
# 

# out_tbl <- read.table(files[1], header=TRUE, sep='|', quote='"',
#                       row.names=FALSE, as.is=TRUE, colClasses=character())

#outfile <- file.path(d_dir, 'National_2015_11_AdHocSQL_all.csv')
#write_csv(tbl, path=outfile)

## cols <- names(out_tbl)

## #lns <- read_lines(files[4])
## #lns[85730:85740]
## for (i in 2:length(files)){
##   cat('Handling file', i, 'of', length(files), '\n')
##   tbl <- read_delim(files[i], delim='|', quote="",
##                     col_types=cols(.default=col_character()))
  
##   if (!all(names(tbl) == cols) ){
##     stop('Names of tables don\'nt match')
##   }
##   out_tbl <- bind_rows(out_tbl, tbl)  
## }



# EOF
