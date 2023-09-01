
# hpms_database_import_2020.R

# 26 Jun 2020
# Matt Landis

# Import HPMS 2020 from https://datahub.transportation.gov/


# Initialize ===============================================================

library('DBI')
library('tidyverse')
library('data.table')
library('stringr')
library('lubridate')
library('RSocrata')
source('db_import/socrata_functions.R')
source('functions/connect_to_db.R')

email = 'joseph.trost@rsginc.com'
password = readRDS('db_import/datahub_pw.rds')

datafields_map = fread('db_import/data_field_changes.csv')

# URLS ========================================================================

# FIXME: Manage URLS in a spreadsheet

# NB!! Specify URLs as JSON, not CSV.  JSON reads all fields as character
# and then converts to the appropriate type whereas CSV guesses datatypes 
# (as in read.csv) and convert later which can result in mistakes

# Note that the cache path is derived from the name of the URL.

# Timelineness
# https://datahub.transportation.gov/Roadways-and-Bridges/HPMS-Submission-Times-2020/cvaj-4fp2/data
timeliness_url = 'https://datahub.transportation.gov/resource/8fiq-jstx.json'

# Samples
# https://datahub.transportation.gov/Roadways-and-Bridges/HPMS-Review-Samples-2020/md8h-imnk
# Click on API

sample_urls = c(
  sample_de = 'https://datahub.transportation.gov/resource/wrs8-irpx.json',
  sample_wy = "https://datahub.transportation.gov/resource/u49a-f2t2.json"
)

# Designations

designation_urls = c(
  designation_de = "https://datahub.transportation.gov/resource/jazs-6frs.json",
  designation_wy = "https://datahub.transportation.gov/resource/b246-kezf.json"
)

# Events

event_urls = c(
  event_de = "https://datahub.transportation.gov/resource/w7if-nkga.json",
  event_wy = "https://datahub.transportation.gov/resource/qdcn-kgx7.json"
)


update_tt = TRUE
update_samples = TRUE
update_sections = TRUE
overwrite_cache = FALSE

# Work ======================================================================

con = odbc::dbConnect(drv = odbc::odbc(), dsn='HPMS9')


# Timeliness table ------------------------------------------------------

if ( update_tt ){
  prod_table = 'Timelinesstable'
  stage_table = 'tt_stage'
  
  tbl = read.socrata(
    url=timeliness_url,
    # app_token=key
    email = email,
    password=password
  )
  
  setDT(tbl)
  str(tbl)
  
  tbl[, year_record := as.integer(year_record)]
  tbl[, state_code := as.integer(state_code)]
  tbl[, submitted_on := mdy_hm(submitted_on)]
  
  dbWriteTable(con, name=stage_table, value=tbl, overwrite=TRUE)
  
  # Check
  tt = tbl(con, from=stage_table)
  glimpse(tt)
  tt %>%
    count(state_code) %>%
    print(n=52)
  
  tt %>%
    count(year_record) %>%
    print(n=52)
  
  
  # Update production table ---------------------------------------------
  
  tbls = DBI::dbListTables(con)
  
  if (!(prod_table %in% tbls)) {

    dbWriteTable(con, name=prod_table, value=tbl, overwrite=TRUE)

  } else {

    # Check for existing current year
    data_yr = year(today()) - 1
    
    tt = tbl(con, from=prod_table)
    
    yr_count = tt %>%
      # filter(Year_Record == data_yr) %>%
      filter(year_record == data_yr) %>%
      count() %>%
      pull(n)
    
    if ( yr_count > 0 ){
      dbExecute(con, paste0('DELETE FROM Timelinesstable WHERE year_record = ',
                            data_yr))
    }
    
    (yr_count = tt %>%
        filter(year_record == data_yr) %>%
        count() %>%
        pull(n))
    
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
        filter(year_record == data_yr) %>%
        count() %>%
        pull(n) ==
        con %>%
        tbl('tt_stage') %>%
        count() %>%
        pull(n)
    )
  }  
}



# Sample sections -------------------------------------------------------

if ( update_samples ){
  
  prod_table = 'Review_Sample_Sections'
  stage_table = 'rss_stage'
  
  
  for ( i in seq_along(sample_urls) ){
    url = sample_urls[i]
    message('Working on ', names(url))
    cache_path = download_socrata(url, overwrite=overwrite_cache)
    counts_local = write_to_stage(cache_path, con, stage_table)
    #copy_rows(con, stage_table, prod_table, counts_local)
  }
  
}


# Sections data ---------------------------------------------------

# NOTE: old (<2022) sections table is concatenation of Events and Designations

if ( update_sections ){
  
  prod_table = 'Review_Sections'
  stage_table = 'rs_stage'
  
  stopifnot( length(designation_urls) == length(event_urls) )
  
  # FIXME: for now testing just with Designations
  # for ( i in seq_along(section_urls) ){
  for ( i in seq_along(designation_urls) ){
    # url = section_urls[i]
    
    url_ev  = event_urls[i]
    url_des = designation_urls[i]
    
    if (url == '') next()
    # cache_path = download_socrata(url, overwrite=overwrite_cache)
    cache_path = create_sections_tables(url_ev, url_des, overwrite=overwrite_cache)
    counts_local = write_to_stage(cache_path, con, stage_table)
    #copy_rows(con, stage_table, prod_table, counts_local)
    
  }
}

dbDisconnect(con)

