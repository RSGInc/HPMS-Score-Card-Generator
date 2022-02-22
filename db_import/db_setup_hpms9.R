# db_setup_hpms9.R

# Set up database with new data items for 2020 and holds 2021 data
# For testing changes for HPMS 9

library(DBI)
library(odbc)
library(tidyverse)
library(stringr)

# Created HPMS9 connection in ODBC Data Sources app

# Create connection
con = odbc::dbConnect(drv = odbc::odbc(), dsn='HPMS9')
con
head(DBI::dbListTables(con))

# Connect to tables
sample_tbl = tbl(src = con, 'Review_Sample_Sections')
section_tbl = tbl(src = con, 'Review_Sections')

# section_tbl %>%
#   count(Year_Record)

# read renames
rename_tbl = read_csv(file = 'db_import/data_item_rename_8_9.csv')


# Rename 2019 except where incompatible

for ( i in 1:nrow(rename_tbl) ){
  
  if ( rename_tbl$comparable[i] == 0 ) next()
  
  name_8 = rename_tbl$name_8[i]
  name_9 = rename_tbl$name_9[i]
  
  message('Renaming ', name_8, ' to ', name_9)
  
  sql = str_glue(
    "update Review_Sections\n",
    "\tset Data_Item = '{name_9}'\n",
    "\twhere Year_Record = 2019 and Data_Item = '{name_8}'")
  
  nrows = dbExecute(con, sql)
  message('Modified ', nrows, ' rows')
  
}

# Rename 2020 
for ( i in 1:nrow(rename_tbl) ){

  from = rename_tbl$name_8[i]
  to = rename_tbl$name_9[i]
  
  message('Renaming ', from, ' to ', to)
  
  sql = str_glue(
    "update Review_Sections\n",
    "\tset Data_Item = '{to}'\n",
    "\twhere Year_Record = 2020 and Data_Item = '{from}'")
  
  sql
  nrows = dbExecute(con, sql)
  message('Modified ', nrows, ' rows')

}

i = 6
(from = rename_tbl$name_8[i])
(to = rename_tbl$name_9[i])

# from = 'TOLL_CHARGED'

section_tbl %>%
  filter(Data_Item == !!from) %>%
  count(Year_Record, State_Code) %>%
  count(Year_Record)
  
section_tbl %>%
  filter(Data_Item == !!to) %>%
  count(Year_Record, State_Code) %>%
  count(Year_Record)

dbDisconnect(con)
