# hpms_database_import.R

# 12 Apr 2017
# Matt Landis

# HPMS data sent to us from justin.clarke@dot.gov as five zipped CSV files.
# This script combines them into a single file for import into a database.


# Initialize ===============================================================
library('RODBC')
library('tidyverse')
library('stringr')
#library('dbplyr') # installed from github
library('readxl')

onedrive <- file.path('C:/Users/matt.landis/OneDrive - Resource Systems Group, Inc')
proj_dir <- file.path(onedrive, 'Projects', 'Fhwa_hpms')
raw_dir <- file.path(proj_dir, 'raw_data')
d_dir <- file.path(proj_dir, 'data')

local_con <- 'Driver={SQL Server Native Client 11.0}; server=10548L\\SQLEXPRESS;database=HPMS2016;trusted_connection=yes;'


# Work ======================================================================


# Load 2015 Sections data --------------------------------------------------

# Push the flat files for sections into a database

choice <- menu(choices = c('Replace it', 'Stop'),
               title='Replacing "Review_Sections" table takes ~ 4 hrs!!')

if ( choice == 1 ){
  files <- Sys.glob(file.path(raw_dir, '*/*.csv'))
  
  # Create connection to database
  
  # Prepare to write to the database
  tablename <- 'Review_sections_staging'
  var_types <- c(Year_Record='smallint',
                 State_Code='smallint',
                 Route_ID='varchar(120)',
                 Begin_Point='decimal(8,3)',
                 End_Point='decimal(8,3)',
                 Section_Length='decimal(8,3)',
                 Data_Item='varchar(25)',
                 Value_Numeric='decimal(10,3)',
                 Value_Text='varchar(50)',
                 Value_Date='datetime',
                 StateYearKey='integer')
  
  
  con <- odbcDriverConnect(connection=local_con)
  
  for ( i in seq_along(files) ){
    
    out_tbl <- read_delim(files[i], delim='|', quote="",
                          col_types=cols(.default=col_character()))
    
    # Rename columns to match schema
    
    out_tbl1 <- out_tbl %>%
      rename(Year_Record = Year_record,
             Route_ID = route_ID,
             Begin_Point = begin_Point,
             End_Point = end_point,
             Data_Item = data_item) %>%
      mutate(Section_Length = as.numeric(End_Point) - as.numeric(Begin_Point),
             StateYearKey = str_c(State_Code, str_sub(Year_Record, start = 3, end=4)))
    
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


# Create 2014, 2015 samples table ===================================================

# The samples table needs to be created from two tables in the HPMS database
# on wrjsqlvdw02.

choice <- menu(choices = c('Replace it', 'Stop'), title='Replacing Review_Sample_Sections!!')

if ( choice == 1 ){
  wrj_con <- 'Driver={SQL Server Native Client 11.0}; server=wrjsqlvdw02;database=HPMS;trusted_connection=yes;'
  
  con <- odbcDriverConnect(connection=wrj_con)
  
  sm_2014 <- sqlFetch(con, 'samples2014', stringsAsFactors=FALSE) %>%
    as_tibble() %>%
    rename(Year_Record = YEAR_RECORD,
           State_Code = STATE_CODE,
           Route_ID = ROUTE_ID,
           Begin_Point = BEGIN_POINT,
           End_Point = END_POINT,
           Sample_ID = SAMPLE_ID,
           Expansion_Factor = EXPANSION_FACTOR) %>%
    mutate(Section_Length = End_Point - Begin_Point,
           StateYearKey = str_c(State_Code, str_sub(Year_Record, start=3, end=4)))
  
  # Check for duplicate rows
  nrow(sm_2014)
  nrow(distinct(sm_2014))
  
  # Drop duplicates
  sm_2014 <- distinct(sm_2014)
  
  # Drop near duplicates
  sm_2014 <- sm_2014 %>%
    group_by(Route_ID, Begin_Point, End_Point, StateYearKey) %>%
    mutate(drop = seq(from=0, to=(n() - 1))) %>%
    ungroup() %>%
    filter(drop == 0) %>%
    select(-drop)
  
  sm_2015 <- sqlFetch(con, 'Samples2015',
                      stringsAsFactors=FALSE) %>%
    as_tibble() %>%
    mutate(Section_Length = End_Point - Begin_Point,
           StateYearKey = str_c(State_Code, str_sub(Year_Record, start=3, end=4)))
  
  nrow(sm_2015)
  nrow(distinct(sm_2015))
  
  sm_tbl <- bind_rows(sm_2014, sm_2015)
  
  close(con)
  
  var_types <- c(Year_Record='smallint',
                 State_Code='smallint',
                 Route_ID='varchar(120)',
                 Begin_Point='decimal(8,3)',
                 End_Point='decimal(8,3)',
                 Section_Length='decimal(8,3)',
                 Sample_ID='varchar(12)',
                 Expansion_Factor='decimal(8,3)',
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
  rename(State_Code = state_Code,
         Data_Item = data_Item,
         Miles = miles,
         Route_ID_Count = route_ID_Count) %>%
  mutate(Year_Record = '2015',
         StateYearKey = str_c(State_Code, as.numeric(Year_Record) %% 100))
  

var_types <- c(Year_Record = 'smallint',
               State_Code = 'smallint',
               Data_Item = 'varchar(25)',
               Record_Count = 'integer',
               Miles = 'decimal(10,3)',
               Route_ID_Count = 'integer',
               StateYearKey='integer')

names(sum_tbl) %in% names(var_types)
names(var_types) %in% names(sum_tbl)

con <- odbcDriverConnect(connection=local_con)
sum_tbl_name <- 'Section_Summaries'
sqlDrop(con, sqtable=sum_tbl_name)
sqlSave(con, dat=sum_tbl, tablename=sum_tbl_name, verbose=FALSE, nastring=NULL,
        varTypes=var_types, fast=TRUE, rownames=FALSE)
close(con)

# In SSMS, I made a primary key out of Year_Record, State_Code, Data_Item, StateYearKey





### unused ---------------------------------------------------------------

# Create primary keys on tables ===========================================

# Did this directly in SSMS. Some code for debugging duplicate keys follows.


# samples table: Route_ID, Begin_Point, End_Point, StateYearKey
# sections table: Route_ID, Begin_Point, End_Point, Data_Item, StateYearKey

# Identify duplicate primary keys in samples table.
# tab <- count(sm_2014, Route_ID, Begin_Point, End_Point, StateYearKey) %>%
#   filter(n > 1)
# tab
# 
# jn <- inner_join(tab, sm_tbl)
# jn
# 
# 
# ex <- tab$Route_ID[1]
# filter(sm_tbl,
#        Route_ID == ex,
#        Begin_Point == 0.779,
#        End_Point == 0.786,
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
