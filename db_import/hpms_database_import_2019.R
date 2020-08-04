
# hpms_database_import_2019.R

# 26 Jun 2020
# Matt Landis

# Import HPMS 2019 from https://datahub.transportation.gov/

# Initialize ===============================================================
library('DBI')
library('tidyverse')
library('data.table')
library('stringr')
library('lubridate')
library('RSocrata')
source('db_import/socrata_functions.R')
source('code/connect_to_db.R')

email = 'matt.landis@rsginc.com'
password = 'tX522b7Dz4xS'

# NB!! Specify URLs as JSON, not CSV.  JSON reads all fields as character
# and then converts to the appropriate type whereas CSV guesses datatypes 
# (as in read.csv) and convert later which can result in mistakes

# Work ======================================================================

con = connect_to_db('burmdlppw01', 'HPMS', intsecurity = TRUE)

# # Timeliness table ------------------------------------------------------
# 
# tbl = read.socrata(
#   url='https://datahub.transportation.gov/resource/hte9-dvcd.json',
#   # app_token=key
#   email = email,
#   password=password
# )
# 
# setDT(tbl)
# str(tbl)
# 
# tbl[, year_record := as.integer(year_record)]
# tbl[, state_code := as.integer(state_code)]
# tbl[, submitted_on := mdy_hm(submitted_on)]
# 
# tbl_name = 'tt_stage'
# dbWriteTable(con, name=tbl_name, value=tbl, overwrite=TRUE)
# 
# # Check
# tt = tbl(con, from=tbl_name)
# glimpse(tt)
# tt %>%
#   count(State_Code) %>%
#   print(n=Inf)
# 
# tt %>%
#   count(Year_Record) %>%
#   print(n=Inf)
# 
# # Update production table ---------------------------------------------
# 
# # Check for existing current year
# 
# tt = tbl(con, from='Timelinesstable')
# 
# yr_count = tt %>%
#   filter(year_record == current_yr) %>%
#   count() %>%
#   pull(n)
# 
# if ( yr_count > 0 ){
#   dbExecute(con, paste0('DELETE FROM Timelinesstable WHERE year_record = ',
#                         current_yr))
# }
# 
# yr_count = tt %>%
#   filter(year_record == current_yr) %>%
#   count() %>%
#   pull(n)
# 
# stopifnot(yr_count== 0)
# 
# old_fields = dbListFields(con, 'Timelinesstable')
# new_fields = dbListFields(con, 'tt_stage')
# 
# setdiff(new_fields, tolower(old_fields))
# setdiff(tolower(old_fields), new_fields)
# 
# # Copy current year into full table.
# sql = paste0('insert into Timelinesstable(', paste(new_fields, collapse=', '),
#              ') select ', paste(new_fields, collapse=', '), ' from tt_stage')
# dbExecute(con, sql)
#           
# 
# stopifnot(
#   con %>%
#     tbl('Timelinesstable') %>%
#     filter(year_record == current_yr) %>%
#     count() %>%
#     pull(n) ==
#     con %>%
#     tbl('tt_stage') %>%
#     count() %>%
#     pull(n)
# )
#
# 
# Sample sections -------------------------------------------------------

# Original data
# url = 'https://datahub.transportation.gov/resource/w6jm-vtp5.json'

# Resubmission
url = 'https://datahub.transportation.gov/resource/b37r-yiaq.json'

ss = read.socrata(url=url, email=email, password=password)
setDT(ss)
str(ss)

tbl_name = 'rss_stage'
dbWriteTable(con, name=tbl_name, value=ss, overwrite=TRUE)

# Check
stage = tbl(con, from=tbl_name)
glimpse(stage)

counts_local = stage %>%
  count(state_code, year_record) %>%
  collect() %>%
  rename(n_local = n)


# Check that we have the right number of rows for each state.

query = paste0(url, '?$query=SELECT state_code,year_record,count(state_code) GROUP BY state_code, year_record ORDER BY state_code, year_record')
counts_remote = read.socrata2(query, email=email, password=password)
setnames(counts_remote, 'count_state_code', 'n_remote')
counts_check = merge(counts_remote, counts_local,
                     by = c('state_code', 'year_record'), all=TRUE)
setDT(counts_check)
stopifnot(counts_check[n_local != n_remote, .N] == 0)


# Move data from stage to production -----------------------------------------

prod_table = 'Review_Sample_Sections'
stage_table = 'rss_stage'

years = unique(counts_local$year_record)
states = unique(counts_local$state_code)

prod = tbl(con, from=prod_table)
n_prod = prod %>% 
  filter(State_Code %in% states, 
         Year_Record %in% years) %>%
  count(State_Code, Year_Record) %>%
  collect()

sql = paste0(
  'DELETE FROM ', prod_table,
  ' WHERE state_code in (', paste(states, collapse=', '), ')',
  ' AND year_record in (', paste(years, collapse=', '), ')')
dbExecute(con, sql)

old_fields = dbListFields(con, prod_table)
new_fields = dbListFields(con, stage_table)

setdiff(tolower(new_fields), tolower(old_fields))
new_fields = new_fields[!new_fields %in% 'natroute_id']

setdiff(tolower(old_fields), tolower(new_fields))


# Create StateYearKey
dbExecute(con, 'alter table rss_stage add StateYearKey as (state_code * 100 + year_record % 1000)')

# Copy current year into full table.
dbExecute(con,
          'insert into Review_Sample_Sections(year_record, state_code, route_id, begin_point, end_point, section_length, sample_id, expansion_factor, stateyearkey)
           select year_record, state_code, route_id, begin_point, end_point,
           section_length, sample_id, expansion_factor, stateyearkey from rss_stage')

counts_prod = prod %>% 
  filter(state_code %in% states,
         year_record %in% years) %>%
  count(state_code, year_record) %>%
  collect()
setnames(counts_prod, 'n', 'n_prod')

counts_check = merge(counts_check, counts_prod,
                     by = c('state_code', 'year_record'), all=TRUE)

stopifnot(all(counts_check[, n_prod] == counts_check[, n_local]))


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

# urls = c(
#   new_england = 'https://datahub.transportation.gov/resource/yed4-dz8a.json',
#   heartland = 'https://datahub.transportation.gov/resource/pu8w-cqik.json',
#   gulf = 'https://datahub.transportation.gov/resource/rf6n-m9pz.json',
#   appalachia = 'https://datahub.transportation.gov/resource/j6bh-426b.json',
#   mid_atlantic = 'https://datahub.transportation.gov/resource/f674-dyn6.json',
#   lakes = 'https://datahub.transportation.gov/resource/xq7x-rndy.json',
#   desert = 'https://datahub.transportation.gov/resource/gwx5-yka7.json',
#   islands = 'https://datahub.transportation.gov/resource/imwx-p856.json',
#   west = 'https://datahub.transportation.gov/resource/pdrm-udaf.json',
#   badlands = 'https://datahub.transportation.gov/resource/gve6-cr9a.json'
# )

# Set url to resubmissions
urls = c(
  resub = 'https://datahub.transportation.gov/resource/yq8j-yqf2.json'
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
  count(state_code, year_record) %>%
  collect() %>%
  rename(n_local = n)


# Check that we have the right number of rows for each state.

counts_remote_list = list()
for ( i in seq_along(urls) ){
  message('Fetching counts for ', names(urls)[i])
  url = paste0(urls[i], '?$query=SELECT state_code,year_record,count(state_code) GROUP BY state_code, year_record ORDER BY state_code, year_record')
  counts_remote_list[[names(urls)[i] ]] = read.socrata2(url, email=email, password=password)
}

counts_remote = rbindlist(counts_remote_list, idcol = 'region')
setnames(counts_remote, 'count_state_code', 'n_remote')

counts_check = merge(counts_remote, counts_local,
                     by = c('state_code', 'year_record'), all=TRUE)

stopifnot(counts_check[n_local != n_remote, .N] == 0)



# Move data from stage to production ------------------------------------------
 

# Review Sections --------------------------------------------------------------

prod_table = 'Review_Sections'
stage_table = 'rs_stage'

years = unique(counts_local$year_record)
states = unique(counts_local$state_code)

prod = tbl(con, from=prod_table)

sql = paste0(
    'DELETE FROM ', prod_table,
    ' WHERE state_code in (', paste(states, collapse=', '), ')',
    ' AND year_record in (', paste(years, collapse=', '), ')')
dbExecute(con, sql)

old_fields = dbListFields(con, prod_table)
new_fields = dbListFields(con, stage_table)

setdiff(tolower(new_fields), tolower(old_fields))
new_fields = new_fields[!new_fields %in% 'natroute_id']

setdiff(tolower(old_fields), tolower(new_fields))

# Create StateYearKey
if ( !'StateYearKey' %in% new_fields ){
  sql = paste0('alter table ', stage_table,
               ' add StateYearKey as (state_code * 100 + year_record % 1000)')
  dbExecute(con, sql)
  new_fields = c(new_fields, 'StateYearKey')
}

# Copy current year into full table.
sql = paste0('insert into ', prod_table, '(', paste(new_fields, collapse=', '),
             ') select ', paste(new_fields, collapse=', '), ' from ', stage_table)
dbExecute(con, sql)

# Get counts in production table
counts_prod = prod %>%
  filter(year_record %in% years,
         state_code %in% states) %>%
  count(state_code, year_record) %>%
  collect() %>%
  rename(n_prod = n)

counts_check = merge(counts_local, counts_prod, by = c('state_code', 'year_record'))
setDT(counts_check)

stopifnot(counts_check[n_local != n_prod, .N] == 0)

dbDisconnect(con)
