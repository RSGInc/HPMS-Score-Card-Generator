# Calculate a summary of the database and compare it to FHWA's summary

library('stringr')
library('tidyverse')
library('RODBC')
library('data.table')

# Get all the rows and then find unique route ids.

# Attempt to import a data file specified by the user
ReadData <- function(state_code, year) {
  
  cat('Fetching the data from the database...')
  con <- GetODBCConnection()
  
  query <- paste0('select YearRecord AS Year_Record, StateId AS State_Code, RouteID AS Route_ID, BeginPoint AS Begin_Point, EndPoint AS End_Point, DataItem AS Data_Item, SectionLength AS Section_Length, ValueNumeric AS Value_Numeric, ValueText AS Value_Text, ValueDate AS Value_Date, StateYearKey from ReviewSections where StateYearKey = ',
                  state_code, as.numeric(year) %% 100)
  
  data <- sqlQuery(con, query, stringsAsFactors=FALSE)
  
  odbcClose(con)
  
  data <- cleanUpQuery(data)
  
  cat('complete!\n')
  
  return(data)
  
}


cleanUpQuery <- function(data){
  # Converts data table from sqlQuery for use in R
  
  # make names lower case
  names(data) <- tolower(names(data))
  
  # if data_item is present, make upper case
  if('data_item' %in% names(data)){
    data$data_item <- toupper(data$data_item)
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

# Get distinct route_ids for a single state, year, data_item

con <- GetODBCConnection()

# state_code <- 34    # NJ
state_code <- 49  # UT

data_item <- 'URBAN_CODE'

query <- str_c("select DISTINCT Route_ID from ReviewSections where Year_Record=2016 and State_Code=", state_code,
               " and Data_Item='", data_item, "';")

sql <- sqlQuery(con, query, stringsAsFactors=FALSE) %>% as_tibble()
names(sql) <- tolower(names(sql))


# Get all the rows just the way the scorecard would do it.
# and then find unique route ids.

## Compare distinct route ids from SQL vs R ---------------------------------

# Get distinct route_ids for a single state, year, data_item


# state_code <- 34    # NJ
# state_code <- 49  # UT
state_code <- 56 # WY

data_item_ <- 'RUTTING'

query <- str_c("select DISTINCT Route_ID from ReviewSections where Year_Record=2016 and State_Code=", state_code,
               " and Data_Item='", data_item_, "';")

con <- GetODBCConnection()
sql <- sqlQuery(con, query, stringsAsFactors=FALSE) %>% as_tibble()
names(sql) <- tolower(names(sql))
odbcClose(con)

r <- ReadData(state_code, 2016) %>% as.data.frame()
r <- r %>% 
  filter(data_item == data_item_) %>% 
  select(route_id) %>% 
  distinct

sql <- sql$route_id
r <- r$route_id

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
# query <- paste('select Year_Record, State_Code, Data_Item,',
#                'COUNT(*) as record_count,',
#                'SUM(Section_Length) as Miles,',
#                'COUNT(DISTINCT Route_ID) as Route_ID_Count',
#                'from ReviewSections',
#                'group by Year_Record, State_Code, Data_Item')
# 
# db <- sqlQuery(con, query, stringsAsFactors=FALSE) %>% as_tibble()
# names(db) <- tolower(names(db))
# 
# 
# odbcClose(con)
# 
# fhwa1 <- fhwa %>%
#   mutate(data_item = tolower(data_item)) %>%
#   select(-stateyearkey) %>%
#   gather(key='measure', value='fhwa', record_count, miles, route_id_count)
# 
# db1 <- db %>%
#   mutate(data_item = tolower(data_item)) %>%
#   filter(year_record == 2015) %>%
#   gather(key='measure', value='from_db', record_count, miles, route_id_count)
# 
# by_vars <- c('year_record', 'state_code', 'data_item')
# 
# no_db <- anti_join(fhwa1, db1, by=by_vars)
# count(no_db, state_code)
# 
# no_fhwa <- anti_join(db1, fhwa1, by=by_vars)
# 
# diffs <- inner_join(fhwa1, db1, by=c('year_record', 'state_code', 'data_item', 'measure')) %>%
#   mutate(diff = from_db - fhwa)
# 
# count(diffs, diff != 0)








