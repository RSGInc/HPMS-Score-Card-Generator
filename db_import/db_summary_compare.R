# Calculate a summary of the database and compare it to FHWA's summary

library('stringr')
library('tidyverse')
library('RODBC')
library('data.table')

# Get all the rows and then find unique route ids.

# Attempt to import a data file specified by the user
ReadData <- function(stateid, year) {
  
  cat('Fetching the data from the database...')
  con <- connect_to_db()
  on.exit(odbcClose(con)
  query <- paste0('select * from Review_Sections where StateYearKey = ',
                  stateid, as.numeric(year) %% 100)
  
  data <- sqlQuery(con, query, stringsAsFactors=FALSE)
  
  data <- cleanUpQuery(data)
  
  cat('complete!\n')
  
  return(data)
  
}


cleanUpQuery <- function(data){
  # Converts data table from sqlQuery for use in R
  
  # make names lower case
  names(data) <- tolower(names(data))
  
  # if dataitem is present, make upper case
  if('dataitem' %in% names(data)){
    data$dataitem <- toupper(data$dataitem)
  }
  
  # Strip white space from text columns so things join properly
  # Also make upper case
  chr_idx <- which(sapply(data, is.character))
  for(j in chr_idx){
    data[, j] <- str_trim(data[, j], side='both')
    data[, j] <- toupper(data[, j])
  }
  
  # Convert to a data.table
  data <- data.table(data)
  
  return(data)
}


## Compare distinct route ids from SQL vs R ---------------------------------

# Get distinct routeids for a single state, year, dataitem

con <- connect_to_db()

# stateid <- 34    # NJ
stateid <- 49  # UT

dataitem <- 'URBAN_ID'

query <- str_c("select DISTINCT RouteId from Review_Sections where DataYear=2016 and StateId=", stateid,
               " and DataItem='", dataitem, "';")

sql <- sqlQuery(con, query, stringsAsFactors=FALSE) %>% as_tibble()
names(sql) <- tolower(names(sql))


# Get all the rows just the way the scorecard would do it.
# and then find unique route ids.

## Compare distinct route ids from SQL vs R ---------------------------------

# Get distinct routeids for a single state, year, dataitem


# stateid <- 34    # NJ
# stateid <- 49  # UT
stateid <- 56 # WY

dataitem_ <- 'RUTTING'

query <- str_c("select DISTINCT RouteId from Review_Sections where DataYear=2016 and StateId=", stateid,
               " and DataItem='", dataitem_, "';")

con <- connect_to_db()
sql <- sqlQuery(con, query, stringsAsFactors=FALSE) %>% as_tibble()
names(sql) <- tolower(names(sql))
odbcClose(con)

r <- ReadData(stateid, 2016) %>% as.data.frame()
r <- r %>% 
  filter(dataitem == dataitem_) %>% 
  select(routeid) %>% 
  distinct

sql <- sql$routeid
r <- r$routeid

sql[!sql %in% r]
r[!r %in% sql]

r <- str_trim(r, side='both')
sql <- str_trim(sql, side='both')

sql[!sql %in% r]
r[!r %in% sql]


# NOT USED ====================================================================
# # Get the FHWA-supplied summary
# query <- paste0('select * from Section_Summaries;')
# fhwa <- sqlQuery(con, query, stringsAsFactors=FALSE) %>% as_tibble()
# 
# names(fhwa) <- tolower(names(fhwa))

# Summarize the database
# query <- paste('select DataYear, StateId, DataItem,',
#                'COUNT(*) as record_count,',
#                'SUM(SectionLength) as Miles,',
#                'COUNT(DISTINCT RouteId) as RouteId_Count',
#                'from Review_Sections',
#                'group by DataYear, StateId, DataItem')
# 
# db <- sqlQuery(con, query, stringsAsFactors=FALSE) %>% as_tibble()
# names(db) <- tolower(names(db))
# 
# 
# odbcClose(con)
# 
# fhwa1 <- fhwa %>%
#   mutate(dataitem = tolower(dataitem)) %>%
#   select(-stateyearkey) %>%
#   gather(key='measure', value='fhwa', record_count, miles, routeid_count)
# 
# db1 <- db %>%
#   mutate(dataitem = tolower(dataitem)) %>%
#   filter(datayear == 2015) %>%
#   gather(key='measure', value='from_db', record_count, miles, routeid_count)
# 
# by_vars <- c('datayear', 'stateid', 'dataitem')
# 
# no_db <- anti_join(fhwa1, db1, by=by_vars)
# count(no_db, stateid)
# 
# no_fhwa <- anti_join(db1, fhwa1, by=by_vars)
# 
# diffs <- inner_join(fhwa1, db1, by=c('datayear', 'stateid', 'dataitem', 'measure')) %>%
#   mutate(diff = from_db - fhwa)
# 
# count(diffs, diff != 0)








