# check_db.R

# A simple R script to check things in the DB

library(dplyr)
library(data.table)
library(stringr)


source('code/connect_to_db.R')

options(scipen=9999)

# State Labels & Codes
gState_Labels <- fread('resources/dat/state_labels.csv')



con = connect_to_db('burmdlppw01', 'HPMS', intsecurity = TRUE)


rs = tbl(con, from='Review_Sections')

rs

# How many Route_ID are as scientific notation?
test = rs %>% 
  select(State_Code, Route_ID) %>%
  distinct() %>%
  collect() %>%
  filter(str_detect(Route_ID, 'e[+][0-9]')) %>%
  count(State_Code)
test

rs %>%
  filter(state_code == 18,
         year_record == 2019) %>%
  count(Data_Item) %>%
  collect()

rdt = rs %>% 
  filter(State_Code == 18,
         Year_Record == 2019) %>%
  select(State_Code, Year_Record, Route_ID) %>%
  count(Route_ID) %>%
  collect()


rss = tbl(con, from = 'Review_Sample_Sections')

sdt = rss %>% 
  filter(State_Code == 18,
               Year_Record == 2019) %>%
  select(State_Code, Year_Record, Route_ID) %>%
  count(Route_ID) %>%
  collect()

  
# Compare route_ids
