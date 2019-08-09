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


d_dir <- file.path('C:/Users/matt.landis/shared_data/HPMS_download_201908')

# Work ======================================================================

# Load 2018 Sections data ---------------------------------------------------

raw_dir <- d_dir
# infile <- file.path(raw_dir, 'HPMS_Sections_w_o_geometry.csv')
infile = file.path(raw_dir, 'HPMS_Review_Sections.csv')
tbl_name <- 'Review_Sections'

# Write file in chunks of 500k lines

inc_rows = 5e5

# Get column types
# read_csv(file = infile, n_max = 100)

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

# # Pick up where it failed
# last_good = 209500000
# # Define callback function that runs after each chunk is read
# chunk_callback = function(x, pos){
#   if (pos < last_good){
#     message('Skipping from row ', pos)
#     return()
#   } else {
#     message('Writing from row ', pos)
#     dbWriteTable(con, name=tbl_name, value=x,
#                  overwrite=(pos == 1), append=(pos > 1))
#   }
# }
# 
# con = connect_to_db('burmdlppw01', 'HPMS_2018', intsecurity = TRUE)
# read_csv_chunked(infile, callback=chunk_callback, chunk_size=inc_rows,
#                  col_types = col_types, progress=show_progress())
# dbDisconnect(con)

# Check the data
con = connect_to_db('burmdlppw01', 'HPMS_2018', intsecurity = TRUE)

rs = tbl(con, from='Review_Sections')

rs %>% 
  count(Year_Record)


# Convert Year_Record to integer

# dbExecute(con, 'alter table Review_Sample_Sections drop column Year_Record2;')
dbExecute(con, 'alter table Review_Sections add Year_Record2 INT')
dbExecute(con, "update Review_Sections
  set Year_Record2 = CONVERT(INT, REPLACE(Year_Record, ',', ''));")

rs %>% 
  count(Year_Record, Year_Record2)

# Replace Year_Record with Year_Record2
dbExecute(con, "alter table Review_Sections
                    drop column Year_Record;")
dbExecute(con, 'alter table Review_Sections add Year_Record INT')
dbExecute(con, "update Review_Sections
                    set Year_Record = Year_Record2")
dbExecute(con, 'alter table Review_Sections drop column year_Record2;')

rs %>%
  count(Year_Record)

# # Load 2018 samples data ----------------------------------------------------

infile = file.path(raw_dir, 'HPMS_Review_Sample_Sections.csv')
tbl_name = 'Review_Sample_Sections'
tbl = fread(infile)
message('\tWriting')
con = connect_to_db('burmdlppw01', 'HPMS_2018', intsecurity = TRUE)
dbWriteTable(con, name=tbl_name, value=tbl, overwrite=TRUE)

rss = tbl(con, from='Review_Sample_Sections')

# Convert Year_Record to integer ------------------------------

rss %>% 
  count(Year_Record)

# dbExecute(con, 'alter table Review_Sample_Sections drop column Year_Record2;')
dbExecute(con, 'alter table Review_Sample_Sections add Year_Record2 INT')
dbExecute(con, "update Review_Sample_Sections
  set Year_Record2 = CONVERT(INT, REPLACE(Year_Record, ',', ''));")

rss %>% 
  count(Year_Record, Year_Record2)

# Replace Year_Record with Year_Record2
dbExecute(con, "alter table Review_Sample_Sections
                    drop column Year_Record;")
dbExecute(con, 'alter table Review_Sample_Sections add Year_Record INT')
dbExecute(con, "update Review_Sample_Sections
                    set Year_Record = Year_Record2")
dbExecute(con, 'alter table Review_Sample_Sections drop column year_Record2;')

rss %>%
  count(Year_Record)

# Convert StateYearKey to integer, dropping commas --------------------------

rss %>%
  count(StateYearKey)

dbExecute(con, "alter table Review_Sample_Sections add StateYearKey2 INT")
dbExecute(con, "update Review_Sample_Sections set StateYearKey2 = convert(int, replace(StateYearKey, ',', ''));")

rss %>%
  count(StateYearKey, StateYearKey2)

# Replace StateYearKey with StateYearKey2
dbExecute(con, "alter table Review_Sample_Sections
                    drop column StateYearKey;")
dbExecute(con, 'alter table Review_Sample_Sections add StateYearKey INT')
dbExecute(con, "update Review_Sample_Sections
                    set StateYearKey = StateYearKey2")
dbExecute(con, 'alter table Review_Sample_Sections drop column StateYearKey2;')

rss %>%
  count(StateYearKey) %>%
  arrange(StateYearKey) %>%
  print(n = Inf)


# Convert Expansion_Factor to double ----------------------------------------

ef = rss %>%
  pull(Expansion_Factor)

test = data.frame(ef, ef2 = as.numeric(ef))
filter(test, is.na(ef2))

dbExecute(con, "alter table Review_Sample_Sections add Expansion_Factor2 float(53)")
dbExecute(con, "update Review_Sample_Sections set Expansion_Factor2 = convert(float(53), replace(Expansion_Factor, ',', ''));")

rss = tbl(con, 'Review_Sample_Sections')
test = rss %>%
  select(Expansion_Factor, Expansion_Factor2) %>%
  collect() 

with(test, all.equal(parse_number(Expansion_Factor), Expansion_Factor2))

# Replace Expansion_Factor with Expansion_Factor2
dbExecute(con, "alter table Review_Sample_Sections
                    drop column Expansion_Factor;")
dbExecute(con, 'alter table Review_Sample_Sections add Expansion_Factor float(53)')
dbExecute(con, "update Review_Sample_Sections
                    set Expansion_Factor = Expansion_Factor2")
dbExecute(con, 'alter table Review_Sample_Sections drop column Expansion_Factor2;')

rss = tbl(con, 'Review_Sample_Sections')

rss
dbDisconnect(con)
