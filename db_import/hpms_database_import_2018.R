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

#q_drive <- '//i-rsg.com/rsgshares/Projects/_Federal/FHWA/15__ HPMS_DataVisualizationSupport'

d_dir <- file.path('C:/Users/matt.landis/shared_data/HPMS_download_201908')

# local_con = 'Driver={SQL Server Native Client 11.0}; server=burmdlppw01;database=HPMS_2018;trusted_connection=yes;'



# Work ======================================================================

# Load 2018 Sections data
raw_dir <- d_dir
# infile <- file.path(raw_dir, 'HPMS_Sections_w_o_geometry.csv')
infile = file.path(raw_dir, 'HPMS_Review_Sections.csv')
tbl_name <- 'Review_Sections'

# Write file in chunks of 500k lines

# max_rows = 117458382  # for no_geom

max_rows = 235380099
inc_rows = 5e5
start_rows = seq(0, max_rows, by = inc_rows)

#con <- odbcDriverConnect(connection=local_con)

# header = names(fread(infile, nrows=1))
# test = fread(infile, nrows=50)

test = read_csv(file = infile, n_max = 100)

col_types = cols(
  Year_Record = col_character(),
  State_Code = col_integer(),
  Route_ID = col_character(),
  Begin_Point = col_double(),
  End_Point = col_double(),
  Data_Item = col_character(),
  Section_Length = col_double(),
  Value_Numeric = col_double(),
  Value_Text = col_character(),
  Value_Date = col_datetime(),
  Comments = col_character(),
  Last_Modified_By = col_skip(),
  Last_Modified_On = col_skip(),
  Data_Source = col_skip(),
  StateYearKey = col_integer(),
  WKT = col_skip(),
  WKB = col_skip()
)
chunk_callback = function(x){
  dbWriteTable(con, name=tbl_name, value=x, overwrite=FALSE, append=TRUE)
}


con = connect_to_db('burmdlppw01', 'HPMS_2018', intsecurity = TRUE)
read_csv_chunked(infile, callback=chunk_callback, chunk_size=inc_rows,
                 col_types = col_types, progress=show_progress())
dbDisconnect(con)

# for ( start in start_rows ){
#   message('Reading row ', start)
#   # tbl = fread(infile, nrows=inc_rows, skip=start, showProgress=TRUE, 
#   #             col.names = header, colClasses='character')
# 
#   tbl = read_csv_chunked(file )
#   message('\tWriting')
#   DBI::dbWriteTable(con, name=tbl_name, value=tbl,
#                     overwrite = (start == 0), append=(start > 0)) 
# }
#   
# dbDisconnect(con)


# # Load 2018 samples data
# infile = file.path(raw_dir, 'HPMS_Review_Sample_Sections.csv')
# tbl_name = 'Review_Sample_Sections'
# tbl = fread(infile)
# message('\tWriting')
# con = connect_to_db('burmdlppw01', 'HPMS_2018', intsecurity = TRUE)
# dbWriteTable(con, name=tbl_name, value=tbl) 
# dbDisconnect(con)

