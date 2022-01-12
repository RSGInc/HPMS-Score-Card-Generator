# fix_IN_route_id.R

# A script to fix the Route_ID field for Indiana.  
# It was read initially as numeric and converted to scientific notation.


library(dplyr)
library(data.table)
library(DBI)
library(stringr)


source('code/connect_to_db.R')

options(scipen=9999)

# State Labels & Codes
gState_Labels <- fread('resources/dat/state_labels.csv')



con = connect_to_db('burmdlppw01', 'HPMS', intsecurity = TRUE)

prod_name = 'ReviewSections'
stage_name = 'rs_stage'

# prod_name = 'Review_Sample_Sections'
# stage_name = 'rss_stage'

prod_tbl = tbl(con, from=prod_name)

prod_tbl

# How many Route_ID are as scientific notation?
state_code = gState_Labels[abbr == 'IN', index]
fixed = prod_tbl %>% 
  # filter(State_Code == state_code) %>%
  filter(Route_ID %like% '%[e][+][0-9]%') %>%
  collect() %>%
  # filter(str_detect(Route_ID, 'e[+][0-9]')) %>%
  mutate(Route_ID_old = Route_ID,
         Route_ID = Route_ID_old %>%
           as.numeric() %>%
           as.character())  

# Check conversion
fixed %>%
  select(Route_ID_old, Route_ID) %>%
  distinct() #%>%
  #View()

# Write to stage table

stage_tbl = tbl(con, from=stage_name)
stage_tbl %>%
  count(state_code, year_record)

message('writing to database')
dbWriteTable(con, stage_name, fixed, overwrite=TRUE)

count_stage = stage_tbl %>%
  count(Year_Record, State_Code) %>%
  collect()

count_stage

# Drop rows from production table ----------------------------------------------

# Verify query returns right number of rows

sql = paste0(
  "SELECT State_Code, Year_Record, COUNT(*) FROM ",
  prod_name,
  " WHERE State_Code in (", paste(count_stage$State_Code, collapse=', '), ")",
  " AND year_record in (", paste(count_stage$Year_Record, collapse=', '), ")",
  " AND Route_ID like '%[e][+][0-9]%'",
  " GROUP BY state_code, year_record")

count_prod = dbGetQuery(con, sql)
count_prod
count_stage

sql = paste0(
  "DELETE FROM ",
  prod_name,
  " WHERE state_code in (", paste(count_stage$State_Code, collapse=', '), ")",
  " AND year_record in (", paste(count_stage$Year_Record, collapse=', '), ")",
  " AND Route_ID like '%[e][+][0-9]%'")

dbExecute(con, sql)


# Copy from stage table to production table ----------------------------------

# Copy current year into full table.
old_fields = dbListFields(con, prod_name)
new_fields = dbListFields(con, stage_name)

setdiff(tolower(new_fields), tolower(old_fields))
new_fields = new_fields[new_fields != 'Route_ID_old']

sql = paste0(
  'insert into ', prod_name, '(', paste(new_fields, collapse=', '),
             ') select ', paste(new_fields, collapse=', '), ' from ', stage_name)
sql
dbExecute(con, sql)

dbDisconnect(con)

