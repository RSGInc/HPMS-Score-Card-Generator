# fix_IN_route_id.R

# A script to fix the RouteId field for Indiana.  
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

prod_name = 'Review_Sections'
stage_name = 'rs_stage'

# prod_name = 'Review_Sample_Sections'
# stage_name = 'rss_stage'

prod_tbl = tbl(con, from=prod_name)

prod_tbl

# How many RouteId are as scientific notation?
stateid = gState_Labels[abbr == 'IN', index]
fixed = prod_tbl %>% 
  # filter(StateId == stateid) %>%
  filter(RouteId %like% '%[e][+][0-9]%') %>%
  collect() %>%
  # filter(str_detect(RouteId, 'e[+][0-9]')) %>%
  mutate(RouteId_old = RouteId,
         RouteId = RouteId_old %>%
           as.numeric() %>%
           as.character())  

# Check conversion
fixed %>%
  select(RouteId_old, RouteId) %>%
  distinct() #%>%
  #View()

# Write to stage table

stage_tbl = tbl(con, from=stage_name)
stage_tbl %>%
  count(stateid, datayear)

message('writing to database')
dbWriteTable(con, stage_name, fixed, overwrite=TRUE)

count_stage = stage_tbl %>%
  count(DataYear, StateId) %>%
  collect()

count_stage

# Drop rows from production table ----------------------------------------------

# Verify query returns right number of rows

sql = paste0(
  "SELECT StateId, DataYear, COUNT(*) FROM ",
  prod_name,
  " WHERE StateId in (", paste(count_stage$StateId, collapse=', '), ")",
  " AND datayear in (", paste(count_stage$DataYear, collapse=', '), ")",
  " AND RouteId like '%[e][+][0-9]%'",
  " GROUP BY stateid, datayear")

count_prod = dbGetQuery(con, sql)
count_prod
count_stage

sql = paste0(
  "DELETE FROM ",
  prod_name,
  " WHERE stateid in (", paste(count_stage$StateId, collapse=', '), ")",
  " AND datayear in (", paste(count_stage$DataYear, collapse=', '), ")",
  " AND RouteId like '%[e][+][0-9]%'")

dbExecute(con, sql)


# Copy from stage table to production table ----------------------------------

# Copy current year into full table.
old_fields = dbListFields(con, prod_name)
new_fields = dbListFields(con, stage_name)

setdiff(tolower(new_fields), tolower(old_fields))
new_fields = new_fields[new_fields != 'RouteId_old']

sql = paste0(
  'insert into ', prod_name, '(', paste(new_fields, collapse=', '),
             ') select ', paste(new_fields, collapse=', '), ' from ', stage_name)
sql
dbExecute(con, sql)

dbDisconnect(con)

