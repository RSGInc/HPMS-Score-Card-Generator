# hpms_database_import_2018.R

# 8 Jun 2017
# Matt Landis

# Import HPMS 2018 data downloaded from 
# https://datahub.transportation.gov/profile/Matt/4mga-ev5x?orderColumn=lastUpdatedDate&orderDirection=desc&tab=sharedToMe


# Initialize ===============================================================
library('DBI')
library('odbc')
library('tidyverse')
library('data.table')
library('stringr')
library('lubridate')

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


raw_dir <- file.path('C:/Users/matt.landis/shared_data/HPMS_download_20191014')

# Work ======================================================================

# Load 2018 Sections data ---------------------------------------------------

infile <- file.path(raw_dir, 'HPMS_Sections_2017-2018.csv')
tbl_name <- 'Review_Sections'

# infile = file.path(raw_dir, 'HPMS_Review_Sections.csv')
# tbl_name <- 'Review_Sections'

# Write file in chunks of 500k lines

inc_rows = 5e5

# Get column types
# read_csv(file = infile, n_max = 100)

col_types = cols(
  DataYear = col_character(),
  StateId = col_integer(),
  RouteId = col_character(),
  BeginPoint = col_double(),
  EndPoint = col_double(),
  DataItem = col_character(),
  SectionLength = col_double(),
  ValueNumeric = col_double(),
  ValueText = col_character(),
  ValueDate = col_datetime(format='%m/%d/%Y %H:%M:%S %p'),
  # Comments = col_character(),
  # LastModifiedBy = col_skip(),
  # LastModifiedOn = col_skip(),
  Data_Source = col_skip(),
  StateYearKey = col_character()
  #WKT = col_skip(),
  #WKB = col_skip()
)

# # Pick up where it failed
# last_good = 209500000

last_good = 0
# Define callback function that runs after each chunk is read
chunk_callback = function(x, pos){
  if (pos < last_good){
    message('Skipping from row ', pos)
    return()
  } else {
    message('\nWriting from row ', pos)
    dbWriteTable(con, name=tbl_name, value=x,
                 overwrite=(pos == 1), append=(pos > 1))
  }
}

con = connect_to_db('burmdlppw01', 'HPMS_2018', intsecurity = TRUE)
read_csv_chunked(infile, callback=chunk_callback, chunk_size=inc_rows,
                 col_types = col_types, progress=show_progress())

# Create StateYearKey
sql = 'alter table Review_Sections add StateYearKey integer;'
dbExecute(con, sql)

sql = 'update Review_Sections
       set StateYearKey = concat(StateId, substring(DataYear, 3, 2));'
dbExecute(con, sql)

dbDisconnect(con)

# Check the data
con = connect_to_db('burmdlppw01', 'HPMS_2018', intsecurity = TRUE)

tbl_name = 'Review_Sections'
rs = tbl(con, from=tbl_name)

rs %>% 
  count(DataYear)


# Load 2018 samples data ----------------------------------------------------

infile = file.path(raw_dir, 'HPMS_Review_Sample_Sections.csv')
tbl_name = 'Review_Sample_Sections'
tbl = fread(infile)
message('\tWriting')
con = connect_to_db('burmdlppw01', 'HPMS_2018', intsecurity = TRUE)
dbWriteTable(con, name=tbl_name, value=tbl, overwrite=TRUE)

rss = tbl(con, from='Review_Sample_Sections')
count(rss)
dbDisconnect(con)
# 
# 
# # Add submissions table ===================================================
# 
# infile = file.path(raw_dir, 'Highway_Performance_Monitoring_System_Submission_Times.csv')
# tbl_name = 'Timelinesstable'
# tbl = fread(infile)
# tbl = tbl %>%
#   mutate(
#     Submitted_On_0 = Submitted_On,
#     Submitted_On = parse_date_time(Submitted_On, orders = 'mdyHMSOp'))
# 
# tbl = select(tbl, -Submitted_On_0)
# con = connect_to_db('burmdlppw01', 'HPMS_2018', intsecurity=TRUE)
# 
# dbWriteTable(con, name=tbl_name, value=tbl, overwrite=TRUE)
# tt = tbl(con, from='Timelinesstable')
# glimpse(tt)
# tt %>%
#   count(StateId) %>%
#   print(n=Inf)
# 
# dbDisconnect(con)
