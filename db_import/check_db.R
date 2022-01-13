# check_db.R

# A simple R script to check things in the DB

library(dplyr)
library(data.table)
library(stringr)


source('code/connect_to_db.R')

options(scipen=9999)

# State Labels & Codes
gState_Labels <- fread('resources/dat/state_labels.csv')

year = 2019
this_abbrev = 'DC'
state_code = gState_Labels[abbr == this_abbrev, index]


con = connect_to_db('burmdlppw01', 'HPMS', intsecurity = TRUE)

rs = tbl(con, from='Review_Sections')
rss = tbl(con, from = 'Review_Sample_Sections')

rs

# Type queries below --------------------------------------------------------
data_item = 'BASE_TYPE'

rs_dt = rs %>%
  filter(Year_Record == year, State_Code == state_code) %>%
  collect() %>%
  data.table()

rs_dt[Data_Item == data_item, .N]



dbDisconnect(con)
