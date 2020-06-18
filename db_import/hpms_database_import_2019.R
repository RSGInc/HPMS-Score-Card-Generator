# hpms_database_import_2019.R

# 8 Jun 2017
# Matt Landis

# Import HPMS 2019 data downloaded from 
# https://datahub.transportation.gov/profile/Matt/4mga-ev5x?orderColumn=lastUpdatedDate&orderDirection=desc&tab=sharedToMe

raw_dir <- file.path('C:/Users/matt.landis/shared_data/HPMS_download_20200611')
sections_file <- file.path(raw_dir, '2019Review_Sections_TestIL.txt')
sample_file = file.path(raw_dir, '2019Review_Sample_Sections_TestIL.txt')

# Initialize ===============================================================
library('DBI')
library('odbc')
library('tidyverse')
library('data.table')
library('stringr')
library('lubridate')
# library('RSocrata')

#' Connect to a database
#'
#' @param server The server to connect to.
#' @param database The name of the database
#' @param intsecurity Logical.  Should the connection use integrated security
#'  (Windows authentication).  Defaults to \code{FALSE}. 
#' @param uid Character.  The username.  If missing and \code{intsecurity} is
#'  \code{FALSE} the user will be prompted.
#' @param pwd Character.  The password.  If missing and \code{intsecurity} is
#'  \code{FALSE} the user will be prompted
#' @param dsn Character.  A dsn defined in the ODBC Data Sources app (on Windows)
#'
#' @return a connection to the specified database.
#' @export
#'
#' @examples
connect_to_db <- function(server,
                          database,
                          intsecurity=FALSE, uid=NULL, pwd=NULL,
                          dsn=NULL){
  
  # Build the connection string
  constring <- paste0('driver={SQL Server};',
                      'server=', server, ';',
                      'database=', database, ';',
                      'port=1433;')
  
  if ( intsecurity ){
    constring <- paste0(constring, 'integrated security=SSPI;')
  } else {
    if ( missing(uid) | is.null(uid) ) uid <- getPass::getPass(msg='Username: ')
    if ( missing(pwd) | is.null(pwd) ) pwd <- getPass::getPass(msg='Password: ')
    
    constring <- paste0(constring, 'uid=', uid, ';', 'pwd=', pwd, ';')  
  }
  
  # See https://db.rstudio.com/dplyr/
  con <- odbc::dbConnect(drv=odbc::odbc(),
                         .connection_string=constring,
                         bigint='numeric')
  return(con)
}

# Work ======================================================================

con = connect_to_db('burmdlppw01', 'HPMS', intsecurity = TRUE)


# Load Sections data ---------------------------------------------------

tbl_name <- 'Review_Sections'

rs = tbl(con, from=tbl_name)
rs %>%
  count(Year_Record)

rs %>% 
  count(Year_Record, State_Code) %>%
  count(Year_Record)

# Get column types
# read_csv(file = sections_file, n_max = 100)

col_types = cols(
  Year_Record = col_integer(),
  State_Code = col_integer(),
  Route_ID = col_character(),
  Begin_Point = col_double(),
  End_Point = col_double(),
  Data_Item = col_character(),
  Section_Length = col_double(),
  Value_Numeric = col_double(),
  Value_Text = col_character(),
  # Value_Date = col_datetime(format='%m/%d/%Y %H:%M:%S %p'),
  Value_Date = col_datetime(format='%Y-%m-%d %H:%M:%S'),
  Comments = col_character(),
  # Last_Modified_By = col_skip(),
  # Last_Modified_On = col_skip(),
  Data_Source = col_skip(),
  StateYearKey = col_character()
  #WKT = col_skip(),
  #WKB = col_skip()
)

sections = read_csv(sections_file, col_types=col_types)
stopifnot(nrow(problems(sections)) == 0)

setDT(sections)

db_fields = dbListFields(con, tbl_name)
fields_not_in_new = setdiff(db_fields, names(sections))
stopifnot(length(fields_not_in_new) == 0)

sections = sections[, db_fields, with=FALSE]

# Check for duplicated rows from previous attempt
to_add = sections[, .(Year_Record, State_Code, Data_Item)] %>%
  unique()

years = to_add[, Year_Record] %>%
  unique() %>%
  paste(collapse=", ")
  
states = to_add[, State_Code] %>%
  unique() %>%
  paste(collapse=', ')

data_items = to_add[, Data_Item] %>%
  unique() %>%
  paste0(collapse="', '") %>%
  paste0("'", ., "'")

# Drop any existing rows that are like what we are about to add
# https://db.rstudio.com/best-practices/run-queries-safely/
sql = str_glue(
  'select count(*) from Review_Sections
    where Year_Record in ({years}) and
          State_Code in ({states}) and
          Data_Item in ({data_items})'
)

n_potential_dupes = dbGetQuery(con, sql)[1, 1]

if ( n_potential_dupes > 0) {
  sql = str_glue(
    'delete from Review_Sections
    where Year_Record in ({years}) and
          State_Code in ({states}) and
          Data_Item in ({data_items})'
  )
  
  nrows = dbExecute(con, sql)
  nrows
}


dbWriteTable(con, name=tbl_name, value=sections, append=TRUE)

rs %>% 
  count(Year_Record, State_Code) %>%
  count(Year_Record)

dbDisconnect(con)

# Write file in chunks of 500k lines

# inc_rows = 5e5

# Define callback function that runs after each chunk is read
# This is set up for repeated efforts to read a very large file into a database
# It isn't for simply appending additional data to an existing table.
# write_chunk = function(x, pos){
#   if (pos < last_good){
#     message('Skipping from row ', pos)
#     return()
#   } else {
#     message('\nWriting from row ', pos)
#     dbWriteTable(con, name=tbl_name, value=x,
#                  overwrite=(pos == 1), append=(pos > 1))
#   }
# }

# # Pick up where it failed
# last_good = 0
# last_good = rs %>%
#   count() %>%
#   collect() %>%
#   pull(n)

# read_csv_chunked(sections_file, callback=write_chunk, chunk_size=inc_rows,
#                  col_types = col_types, progress=show_progress())


# # Create StateYearKey
# sql = 'alter table Review_Sections add StateYearKey integer;'
# dbExecute(con, sql)
# 
# sql = 'update Review_Sections
#        set StateYearKey = concat(State_Code, substring(Year_Record, 3, 2));'
# dbExecute(con, sql)
# 
# dbDisconnect(con)

# Check the data
# rs = tbl(con, from=tbl_name)
# 
# rs %>% 
#   count(Year_Record)


# Load 2019 samples data ----------------------------------------------------

tbl_name = 'Review_Sample_Sections'
tbl = fread(sample_file)

con = connect_to_db('burmdlppw01', 'HPMS', intsecurity = TRUE)

rss = tbl(con, from=tbl_name)

rss %>%
  count(Year_Record, State_Code) %>%
  count(Year_Record)

# Check for extra/missing fields
db_fields = dbListFields(con, tbl_name)
fields_not_in_new = setdiff(db_fields, names(tbl))
stopifnot(length(fields_not_in_new) == 0)


# Check for duplicated rows from previous attempt

to_add = tbl[, .(Year_Record, State_Code)] %>%
  unique()

years = to_add[, Year_Record] %>%
  unique() %>%
  paste(collapse=", ")

states = to_add[, State_Code] %>%
  unique() %>%
  paste(collapse=', ')

# Drop any existing rows that are like what we are about to add
# https://db.rstudio.com/best-practices/run-queries-safely/
sql = str_glue(
  'select count(*) from {tbl_name}
    where Year_Record in ({years}) and
          State_Code in ({states})'
)

n_potential_dupes = dbGetQuery(con, sql)[1, 1]

if ( n_potential_dupes > 0) {
  sql = str_glue(
    'delete from {tbl_name}
    where Year_Record in ({years}) and
          State_Code in ({states})'
  )
  
  nrows = dbExecute(con, sql)
  nrows
}




message('\tWriting')
tbl = tbl[, db_fields, with=FALSE]
dbWriteTable(con, name=tbl_name, value=tbl, append=TRUE)

rss %>%
  count(Year_Record, State_Code) %>%
  count(Year_Record)

dbDisconnect(con)


# Add submissions table ===================================================

# # Test RSocrata
# 
# # Download the timelineness table based on it's API endpoint URL. 
# # See https://dev.socrata.com/consumers/getting-started.html
# 

key = 'v0oozgxsfrtlvglgo2gj2td8'
email = 'matt.landis@rsginc.com'
password = 'tX522b7Dz4xS'

tbl = read.socrata(
  url='https://datahub.transportation.gov/resource/hte9-dvcd.json',
  # app_token=key
  email = email,
  password=password
)

# # Download from https://datahub.transportation.gov/Roadways-and-Bridges/View-based-on-Highway-Performance-Monitoring-Syste/hte9-dvcd
# infile = file.path(raw_dir, 'Highway_Performance_Monitoring_System_Submission_Times.csv')
# tbl = fread(infile)

tbl = tbl %>%
  mutate(submitted_on = mdy_hms(submitted_on))

con = connect_to_db('burmdlppw01', 'HPMS', intsecurity=TRUE)

tbl_name = 'Timelinesstable'
dbWriteTable(con, name=tbl_name, value=tbl, append=TRUE)
tt = tbl(con, from='Timelinesstable')
glimpse(tt)
tt %>%
  count(State_Code) %>%
  print(n=Inf)

tt %>%
  count(Year_Record) %>%
  print(n=Inf)

dbDisconnect(con)
