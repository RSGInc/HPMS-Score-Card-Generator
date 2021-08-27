
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

email = 'matt.landis@rsginc.com'
password = 'tX522b7Dz4xS'

# URLS ========================================================================

# NB!! Specify URLs as JSON, not CSV.  JSON reads all fields as character
# and then converts to the appropriate type whereas CSV guesses datatypes 
# (as in read.csv) and convert later which can result in mistakes

# Timelineness
# https://datahub.transportation.gov/Roadways-and-Bridges/HPMS-Submission-Times-2020/cvaj-4fp2/data
timeliness_url = 'https://datahub.transportation.gov/resource/cvaj-4fp2.json'

# Samples
# https://datahub.transportation.gov/Roadways-and-Bridges/HPMS-Review-Samples-2020/md8h-imnk
# Click on API

sample_urls = c(
  # sample = 'https://datahub.transportation.gov/resource/md8h-imnk.json'
  sample_update = 'https://datahub.transportation.gov/resource/hatx-u24t.json'
  )

# Sections
section_urls = c(
  # new_england = 'https://datahub.transportation.gov/resource/hbyg-7iey.json',
  # heartland = 'https://datahub.transportation.gov/resource/jh87-u8fg.json',
  # gulf = 'https://datahub.transportation.gov/resource/vjhv-he2m.json',
  # appalachia = 'https://datahub.transportation.gov/resource/5kg6-4nxs.json',
  # mid_atlantic = 'https://datahub.transportation.gov/resource/g4cy-ux2k.json',
  # lakes = 'https://datahub.transportation.gov/resource/uvrx-u32d.json',
  # desert = 'https://datahub.transportation.gov/resource/pa99-vcx4.json',
  # islands = 'https://datahub.transportation.gov/resource/mcd2-efbe.json',
  # west = 'https://datahub.transportation.gov/resource/cub3-t2sg.json',
  # badlands = 'https://datahub.transportation.gov/resource/act5-byyg.json'
  
  # Resubmissions ---------------------------------------------------------

  FL = 'https://datahub.transportation.gov/resource/jyug-rjx7.json',
  GALA = 'https://datahub.transportation.gov/resource/ce8s-sd58.json',
  NYPA = 'https://datahub.transportation.gov/resource/xvft-4wcn.json',
  CAORWA = 'https://datahub.transportation.gov/resource/wuzj-sjt6.json',
  IN = 'https://datahub.transportation.gov/resource/cm27-4cra.json',
  MIOH = 'https://datahub.transportation.gov/resource/up53-htsc.json',
  PR = 'https://datahub.transportation.gov/resource/3kmm-pfkb.json',
  NE = 'https://datahub.transportation.gov/resource/qn8j-bvka.json',
  HI = 'https://datahub.transportation.gov/resource/93xt-8hse.json',
  TN = 'https://datahub.transportation.gov/resource/3u5b-a5n4.json'
  
)

update_tt = FALSE
update_samples = TRUE
update_sections = TRUE
overwrite_cache = FALSE

# Work ======================================================================

con = connect_to_db('burmdlppw01', 'HPMS', intsecurity = TRUE)


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
  
  # Check for existing current year
  data_yr = year(today()) - 1
  
  tt = tbl(con, from=prod_table)
  
  yr_count = tt %>%
    filter(Year_Record == data_yr) %>%
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



# Sample sections -------------------------------------------------------

if ( update_samples ){
  
  prod_table = 'Review_Sample_Sections'
  stage_table = 'rss_stage'
  
  
  for ( i in seq_along(sample_urls) ){
    url = sample_urls[i]
    message('Working on ', names(url))
    cache_path = download_socrata(url, overwrite=overwrite_cache)
    counts_local = write_to_stage(cache_path, con, stage_table)
    copy_rows(con, stage_table, prod_table, counts_local)
  }
  
}


# Sections data ---------------------------------------------------

if ( update_sections ){
  
  prod_table = 'Review_Sections'
  stage_table = 'rs_stage'
  
  for ( i in seq_along(section_urls) ){
    url = section_urls[i]
    if (url == '') next()
    cache_path = download_socrata(url, overwrite=overwrite_cache)
    counts_local = write_to_stage(cache_path, con, stage_table)
    copy_rows(con, stage_table, prod_table, counts_local)
    
  }
}

dbDisconnect(con)

