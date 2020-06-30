
# hpms_database_import_2019.R

# 26 Jun 2020
# Matt Landis

# Import HPMS 2019 from https://datahub.transportation.gov/

# Initialize ===============================================================
library('DBI')
library('odbc')
library('tidyverse')
library('data.table')
library('stringr')
library('lubridate')
library('RSocrata')
source('db_import/socrata_functions.R')

email = 'matt.landis@rsginc.com'
password = 'tX522b7Dz4xS'


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


raw_dir <- file.path('C:/Users/matt.landis/shared_data/HPMS_download_20200626')

# Work ======================================================================

con = connect_to_db('burmdlppw01', 'HPMS', intsecurity = TRUE)


# Timeliness table ------------------------------------------------------

tbl = read.socrata(
  url='https://datahub.transportation.gov/resource/hte9-dvcd.json',
  # app_token=key
  email = email,
  password=password
)

setDT(tbl)
str(tbl)

tbl[, year_record := as.integer(year_record)]
tbl[, state_code := as.integer(state_code)]
tbl[, submitted_on := mdy_hm(submitted_on)]

tbl_name = 'tt_stage'
dbWriteTable(con, name=tbl_name, value=tbl, overwrite=TRUE)

# Check
tt = tbl(con, from=tbl_name)
glimpse(tt)
tt %>%
  count(State_Code) %>%
  print(n=Inf)

tt %>%
  count(Year_Record) %>%
  print(n=Inf)


# Sample sections -------------------------------------------------------

url = 'https://datahub.transportation.gov/resource/w6jm-vtp5.csv'

ss = read.socrata(url=url, email=email, password=password)
setDT(ss)
str(ss)

tbl_name = 'rss_stage'
dbWriteTable(con, name=tbl_name, value=ss, overwrite=TRUE)

# Check
ss = tbl(con, from=tbl_name)
glimpse(ss)


# Load Sections data ---------------------------------------------------

tbl_name <- 'rs_stage'


# For checking column types
col_type_chk = c(
  year_record = 'integer',
  state_code = 'integer',
  route_id = 'character',
  begin_point = 'numeric',
  end_point = 'numeric',
  data_item = 'character',
  value_numeric = 'numeric',
  value_text = 'character',
  value_date = 'POSIXct',
  natroute_id = 'character'
)

coltype_chk_dt = data.table(field = names(col_type_chk), chk = col_type_chk)

urls = c(
  new_england = 'https://datahub.transportation.gov/resource/yed4-dz8a.csv',
  heartland = 'https://datahub.transportation.gov/resource/pu8w-cqik.csv',
  gulf = 'https://datahub.transportation.gov/resource/rf6n-m9pz.csv',
  appalachia = 'https://datahub.transportation.gov/resource/j6bh-426b.csv',
  mid_atlantic = 'https://datahub.transportation.gov/resource/f674-dyn6.csv',
  lakes = 'https://datahub.transportation.gov/resource/xq7x-rndy.csv',
  desert = 'https://datahub.transportation.gov/resource/gwx5-yka7.csv',
  islands = 'https://datahub.transportation.gov/resource/imwx-p856.csv',
  west = 'https://datahub.transportation.gov/resource/pdrm-udaf.csv',
  badlands = 'https://datahub.transportation.gov/resource/gve6-cr9a.csv'
)


for ( i in seq_along(urls) ){
  message('Downloading ', names(urls)[i])
  dt = read.socrata(url=urls[i], email=email, password=password)
  setDT(dt)
  
  if ( names(urls)[i] == 'mid_atlantic' ){
    dt[, value_date := ymd_hms(value_date)]
  }
  
  col_type_obs = sapply(dt, function(x) class(x)[1])
  coltype_obs_dt = data.table(field = names(col_type_obs), obs = col_type_obs)
  
  coltype_dt = merge(coltype_chk_dt, coltype_obs_dt, by='field')
  
  if ( coltype_dt[chk != obs, .N] > 0 ){
    message('col type mismatch - writing to file')
    filename = file.path(raw_dir, paste0(names(urls)[i], '.rds'))
    saveRDS(dt, filename)
  } else {
    message('writing to database')
    dbWriteTable(con, tbl_name, dt, append=TRUE)
  }
  
}
# Check
dt = tbl(con, from=tbl_name)
glimpse(dt)

counts_local = dt %>%
  count(state_code) %>%
  collect() %>%
  rename(n_local = n)


# Check that we have the right number of rows for each state.

counts_remote_list = list()
for ( i in seq_along(urls) ){
  message('Fetching counts for ', names(urls)[i])
  url = URLencode(paste0(urls[i], '?$select=state_code,data_item'))
  url = paste0(urls[i], '?$query=SELECT state_code, count(state_code) GROUP BY state_code ORDER BY state_code')
  counts_remote_list[[names(urls)[i] ]] = read.socrata2(url, email=email, password=password)
}

counts_remote = rbindlist(counts_remote_list, idcol = 'region')
setnames(counts_remote, 'count_state_code', 'n_remote')

counts_check = merge(counts_remote, counts_local, by = 'state_code', all=TRUE)

stopifnot(counts_check[n_local != n_remote, .N] == 0)


# Move data from stage table to production table =======================

con = connect_to_db('burmdlppw01', 'HPMS', intsecurity = TRUE)
current_yr = 2019


# Timelinesstable --------------------------------------------------------------

# Check for existing current year

tt = tbl(con, from='Timelinesstable')

yr_count = tt %>%
  filter(year_record == current_yr) %>%
  count() %>%
  pull(n)

if ( yr_count > 0 ){
  dbExecute(con, paste0('DELETE FROM Timelinesstable WHERE year_record = ',
                        current_yr))
}

yr_count = tt %>%
  filter(year_record == current_yr) %>%
  count() %>%
  pull(n)

stopifnot(yr_count== 0)

old_fields = dbListFields(con, 'Timelinesstable')
new_fields = dbListFields(con, 'tt_stage')

setdiff(new_fields, tolower(old_fields))
setdiff(tolower(old_fields), new_fields)

# Copy current year into full table.
sql = paste0('insert into Timelinesstable(', paste(new_fields, collapse=', '),
             ') select ', paste(new_fields, collapse=', '), ' from tt_stage')
dbExecute(con, sql)
          

stopifnot(
  con %>%
    tbl('Timelinesstable') %>%
    filter(year_record == current_yr) %>%
    count() %>%
    pull(n) ==
    con %>%
    tbl('tt_stage') %>%
    count() %>%
    pull(n)
)



# Sample Sections --------------------------------------------------------------


ss = tbl(con, from='Review_Sample_Sections')

yr_count = ss %>%
  filter(year_record == current_yr) %>%
  count() %>%
  pull(n)

if ( yr_count > 0 ){
  dbExecute(con, paste0('DELETE FROM Review_Sample_Sections WHERE year_record = ',
                             current_yr))
}

yr_count = ss %>%
  filter(year_record == 2019) %>%
  count() %>%
  pull(n)

stopifnot(yr_count== 0)

old_fields = dbListFields(con, 'Review_Sample_Sections')
new_fields = dbListFields(con, 'rss_stage')

setdiff(new_fields, tolower(old_fields))

# Create StateYearKey
dbExecute(con, 'alter table rss_stage add StateYearKey as (state_code * 100 + year_record % 1000)')

# Copy current year into full table.
dbExecute(con,
          'insert into Review_Sample_Sections(year_record, state_code, route_id, begin_point, end_point, section_length, sample_id, expansion_factor, stateyearkey) 
           select year_record, state_code, route_id, begin_point, end_point,
           section_length, sample_id, expansion_factor, stateyearkey from rss_stage')

stopifnot(
  con %>%
      tbl('Review_Sample_Sections') %>%
      filter(year_record == current_yr) %>%
      count() %>%
      pull(n) ==
    con %>%
      tbl('rss_stage') %>%
      count() %>%
      pull(n)
)



# Review Sections --------------------------------------------------------------

prod_table = 'Review_Sections'
stage_table = 'rs_stage'

prod = tbl(con, from=prod_table)

yr_count = prod %>%
  filter(year_record == current_yr) %>%
  count() %>%
  pull(n)

if ( yr_count > 0 ){
  sql = paste0('DELETE FROM ', prod_table, ' WHERE year_record = ',
               current_yr)
  dbExecute(con, sql)
}

yr_count = prod %>%
  filter(year_record == current_yr) %>%
  count() %>%
  pull(n)

stopifnot(yr_count== 0)

old_fields = dbListFields(con, prod_table)
new_fields = dbListFields(con, stage_table)

setdiff(new_fields, tolower(old_fields))
new_fields = new_fields[!new_fields %in% 'natroute_id']

setdiff(tolower(old_fields), new_fields)

# Create StateYearKey
sql = paste0('alter table ', stage_table,
             ' add StateYearKey as (state_code * 100 + year_record % 1000)')
dbExecute(con, sql)


# Copy current year into full table.
sql = paste0('insert into ', prod_table, '(', paste(new_fields, collapse=', '),
             ') select ', paste(new_fields, collapse=', '), ' from ', stage_table)
dbExecute(con, sql)


stopifnot(
  con %>%
    tbl(prod_table) %>%
    filter(year_record == current_yr) %>%
    count() %>%
    pull(n) ==
    con %>%
    tbl(stage_table) %>%
    count() %>%
    pull(n)
)

dbDisconnect(con)
