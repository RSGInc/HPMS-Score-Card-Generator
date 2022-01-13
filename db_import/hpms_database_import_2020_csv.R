
# hpms_database_import_2020.R

# 26 Jun 2020
# Matt Landis

# Import HPMS 2020 from https://datahub.transportation.gov/

# Initialize ===============================================================
library('DBI')
library('data.table')
library('dplyr')
library('stringr')
library('lubridate')
library('RSocrata')
source('db_import/socrata_functions.R')
source('functions/connect_to_db.R')

# Functions =================================================================

csv_to_stage = function(file, con, stage_table){

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
  
  dt = fread(file)
  names(dt) = str_to_lower(names(dt))
  
  counts_remote = dt[, .(n_remote = .N), keyby=.(state_code, year_record)]
  
  # Compare datatypes ------------
  
  dt[, route_id := as.character(route_id)]
  
  if (class(dt$value_date)[1] == 'character'){
    dt[, value_date := ymd_hms(value_date)]
  }
  
  col_type_obs = sapply(dt, function(x) class(x)[1])
  coltype_obs_dt = data.table(field = names(col_type_obs), obs = col_type_obs)
  
  coltype_dt = merge(coltype_chk_dt, coltype_obs_dt, by='field', all=TRUE)
  
  if ( coltype_dt[chk != obs, .N] > 0 ){
    
    message('Column type mismatch')
    print(coltype_dt)
    browser()
    
  }
  
  # Compare column names --------------
  fields_local = dbListFields(con, stage_table)
  fields_remote = names(dt)
  
  not_in_local = setdiff(fields_remote, fields_local)
  if ( length(not_in_local) > 0 ){
    dt[, (not_in_local) := NULL]
  }

  # Write to the database -----------------
  
  message('writing to database')
  
  states = unique(dt[, state_code])
  years = unique(dt[, year_record])
  
  message('States = ', paste(states, collapse=', '))
  message('Years = ', paste(years, collapse=', '))
  
  if (stage_table %in% dbListTables(conn = con) ){
    sql = paste0(
      'DELETE FROM ', stage_table,
      ' WHERE state_code in (', paste(states, collapse=', '), ')',
      ' AND year_record in (', paste(years, collapse=', '), ')')
    dbExecute(con, sql)
  }
  
  dbWriteTable(con, stage_table, dt, append=TRUE)
  message('Done!')
  
  
  # Check
  
  dt_stage = tbl(con, from=stage_table)
  # glimpse(dt)
  
  # Check that we have the right number of rows for each state.
  
  counts_local = dt_stage %>%
    count(state_code, year_record) %>%
    collect() %>%
    rename(n_local = n)
  
  counts_check = merge(counts_remote, counts_local,
                       by = c('state_code', 'year_record'), all=TRUE)
  setDT(counts_check)
  stopifnot(counts_check[n_local != n_remote, .N] == 0)
  
  return(counts_local)
  
}

socrata_to_stage = function(url, con, stage_table){
  
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
  
  
  query = paste0(
    url,
    '?$query=SELECT state_code,year_record,count(state_code) ',
    'GROUP BY state_code, year_record ',
    'ORDER BY state_code, year_record')
  
  counts_remote = read.socrata2(query, email=email, password=password)
  setnames(counts_remote, 'count_state_code', 'n_remote')
  
  # if (!str_detect(url, 'json$|csv$')){
  #   url = str_c(url, '.json')
  # }
  
  message('Downloading ', sum(counts_remote[, 'n_remote']), ' rows from ', names(url))
  
  dt = read.socrata(url=url, email=email, password=password)
  setDT(dt)
  
  message('Downloaded...')
  
  #if ( names(url) %in% c('mid_atlantic', 'SC', 'AZ', 'TN') ){
  if (class(dt$value_date) == 'character'){
    dt[, value_date := ymd_hms(value_date)]
  }
  
  col_type_obs = sapply(dt, function(x) class(x)[1])
  coltype_obs_dt = data.table(field = names(col_type_obs), obs = col_type_obs)
  
  coltype_dt = merge(coltype_chk_dt, coltype_obs_dt, by='field')
  
  if ( coltype_dt[chk != obs, .N] > 0 ){
    
    message('Column type mismatch')
    print(coltype_dt[chk != obs])
    browser()

  }
    
  message('writing to database')
  
  states = unique(dt[, 'state_code'])
  years = unique(dt[, 'year_record'])
  
  if (stage_table %in% dbListTables(conn = con) ){
    sql = paste0(
      'DELETE FROM ', stage_table,
      ' WHERE state_code in (', paste(states, collapse=', '), ')',
      ' AND year_record in (', paste(years, collapse=', '), ')')
    dbExecute(con, sql)
  }
  
  dbWriteTable(con, stage_table, dt, append=TRUE)
  message('Done!')

  
  # Check
  
  dt_stage = tbl(con, from=stage_table)
  # glimpse(dt)
  
  # Check that we have the right number of rows for each state.
  
  counts_local = dt_stage %>%
    count(state_code, year_record) %>%
    collect() %>%
    rename(n_local = n)
  
  counts_check = merge(counts_remote, counts_local,
                       by = c('state_code', 'year_record'), all=TRUE)
  setDT(counts_check)
  stopifnot(counts_check[n_local != n_remote, .N] == 0)
  
  return(counts_local)
}

copy_rows = function(con, prod_table, stage_table){

  # Move data from stage to production -----------------------------------------
  
  stage = tbl(con, from=stage_table)
  
  counts_stage = stage %>%
    count(year_record, state_code)
  
  years = unique(counts_stage$year_record)
  states = unique(countes_stage$state_code)
  
  old_fields = dbListFields(con, prod_table)
  new_fields = dbListFields(con, stage_table)

  # Create StateYearKey
  if ( !'StateYearKey' %in% new_fields ){
    sql = paste0('alter table ', stage_table,
                 ' add StateYearKey as (state_code * 100 + year_record % 1000)')
    dbExecute(con, sql)
    new_fields = c(new_fields, 'StateYearKey')
  }
  
  # setdiff(tolower(new_fields), tolower(old_fields))
  new_fields = new_fields[!new_fields %in% 'natroute_id']
  
  # stopifnot(
  #   length(setdiff(tolower(new_fields), tolower(old_fields))) == 0,
  #   length(setdiff(tolower(old_fields), tolower(new_fields))) == 0
  # )
  
  # Copy rows into production table.
  
  sql = paste0(
    'DELETE FROM ', prod_table,
    ' WHERE state_code in (', paste(states, collapse=', '), ')',
    ' AND year_record in (', paste(years, collapse=', '), ')')
  dbExecute(con, sql)
  
  message('Copying from stage to production')
  sql = paste0('insert into ', prod_table, '(', paste(new_fields, collapse=', '),
               ') select ', paste(new_fields, collapse=', '), ' from ', stage_table)
  dbExecute(con, sql)
  message('Done')
  
  # Get counts in production table
  counts_prod = prod %>%
    filter(year_record %in% years,
           state_code %in% states) %>%
    count(State_Code, Year_Record) %>%
    collect() %>%
    rename(n_prod = n)
  
  names(counts_prod) = str_to_lower(names(counts_prod))
  
  counts_check = merge(counts_local, counts_prod, by = c('state_code', 'year_record'))
  setDT(counts_check)
  
  stopifnot(counts_check[n_local != n_prod, .N] == 0)
  
  sql = paste0(
    'DELETE FROM ', stage_table,
    ' WHERE state_code in (', paste(states, collapse=', '), ')',
    ' AND year_record in (', paste(years, collapse=', '), ')')
  dbExecute(con, sql)
  
}


email = 'matt.landis@rsginc.com'
password = 'tX522b7Dz4xS'

# NB!! Specify URLs as JSON, not CSV.  JSON reads all fields as character
# and then converts to the appropriate type whereas CSV guesses datatypes 
# (as in read.csv) and convert later which can result in mistakes


# Work ======================================================================

con = connect_to_db('burmdlppw01', 'HPMS', intsecurity = TRUE)

# Sample sections -------------------------------------------------------

prod_table = 'Review_Sample_Sections'
stage_table = 'rss_stage'

# For Testing 2020 data, we received CSV files
sample_files = c(
  MO = 'data/MOVASamples.csv'
)

for ( i in seq_along(sample_files) ){
  file = sample_files[i]
  message('Working on ', names(file))
  counts_local = socrata_to_stage(file, con, stage_table)
  copy_rows(con, prod_table, stage_table, counts_local)
}
message('Success!')


# # Original data
# # url = 'https://datahub.transportation.gov/resource/w6jm-vtp5.json'
# 
# sample_urls = c(
#   WA = 'https://datahub.transportation.gov/Roadways-and-Bridges/WashingtonSample/abr7-b6qb',
#   CA = 'https://datahub.transportation.gov/dataset/CaliforniaSample/uh86-bwdf'
# )
# 
# 
# for ( i in seq_along(sample_urls) ){
#   url = sample_urls[i]
#   message('Working on ', names(url))
#   counts_local = socrata_to_stage(url, con, stage_table)
#   copy_rows(con, prod_table, stage_table, counts_local)
# }



# Sections data ---------------------------------------------------

prod_table = 'Review_Sections'
stage_table = 'rs_stage'

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
# urls = c(
#   resub = 'https://datahub.transportation.gov/resource/yq8j-yqf2.json'
# )

# Resubmissions on 20 Aug 2020

# sections_urls = c(
#   # MN = 'https://datahub.transportation.gov/Roadways-and-Bridges/MinnesotaSections2019/3uf9-qbi2',
#   # MD = 'https://datahub.transportation.gov/Roadways-and-Bridges/MarylandSections2019/ddzr-fkg3',
#   # NM = 'https://datahub.transportation.gov/dataset/HPMSNewMexicoSections2019/9tf9-8qq4',
#   # SC = 'https://datahub.transportation.gov/dataset/HPMSSouthCarolinaSections2019/axzv-af4r',
#   # AZ = 'https://datahub.transportation.gov/dataset/HPMSArizonaSections2019/undi-6wdr',
#   # TN = 'https://datahub.transportation.gov/dataset/HPMSTennesseeSections2019/djic-ekmg',
#   # PR = 'https://datahub.transportation.gov/dataset/HPMSPuertoRicoSections2019/yrrm-p8fu',
#   ND = 'https://datahub.transportation.gov/dataset/HPMSNorthDakotaSections2019/7v4t-66uv',
#   LA = 'https://datahub.transportation.gov/dataset/HPMSLouisianaSections2019/6h5i-ej6p',
#   HI = 'https://datahub.transportation.gov/dataset/HPMSHawaiiSections2019/tuf3-6qxe',
#   CA = 'https://datahub.transportation.gov/dataset/HPMSCaliforniaSections2019/mydz-ydth'
# )

# # Resubmissions on 21 Sep 2020
# sections_urls = c(
#   WA = 'https://datahub.transportation.gov/Roadways-and-Bridges/ScoreCardWashingtonSections/ydyu-q73e',
#   CA = 'https://datahub.transportation.gov/dataset/ScorecardCaliforniaSections/pcvt-4zpt'
# )
# 
# for ( i in seq_along(sections_urls) ){
#   url = sections_urls[i]
#   counts_local = socrata_to_stage(url, con, stage_table)
#   copy_rows(con, prod_table, stage_table, counts_local)
#   
# }

# For Testing 2020 data, we received CSV files
sample_files = c(
  MO = 'data/MO Sections 2020 Data.csv',
  VA = 'data/VA Sections 2020 Data.csv'
)

for ( i in seq_along(sample_files) ){
  file = sample_files[i]
  message('Working on ', names(file))
  counts_local = csv_to_stage(file, con, stage_table)
  copy_rows(con, prod_table, stage_table, counts_local)
}

message('Success!\n\n')


dbDisconnect(con)

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
# 
# tbl = fread('data/2020 Submission Times.csv')
# str(tbl)
# 
# tbl[, Year_Record := as.integer(Year_Record)]
# tbl[, State_Code := as.integer(State_Code)]
# tbl[, Submitted_On := mdy_hm(Submitted_On)]
# 
# stage_table = 'tt_stage'
# dbWriteTable(con, name=stage_table, value=tbl, overwrite=TRUE)
# 
# # Check
# tt = tbl(con, from=stage_table)
# glimpse(tt)
# tt %>%
#   count(State_Code) %>%
#   print()
# 
# tt %>%
#   count(Year_Record) %>%
#   print(n)
# 
# 
# # Update production table ---------------------------------------------
# 
# # Check for existing current year
# current_yr = 2020
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
# setdiff(new_fields, old_fields)
# setdiff(old_fields, new_fields)
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
