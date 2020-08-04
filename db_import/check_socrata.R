# check_db.R

# A simple R script to check things in the Socrata database

library('data.table')
library('stringr')
library('lubridate')
library('RSocrata')
source('db_import/socrata_functions.R')

email = 'matt.landis@rsginc.com'
password = 'tX522b7Dz4xS'


# 2019 URLS
urls = c(
  
  # Timeliness table
  time = 'https://datahub.transportation.gov/resource/hte9-dvcd.json',
  
  # Review Sample Sections
  rss = 'https://datahub.transportation.gov/resource/w6jm-vtp5.json',
  rss_resub = 'https://datahub.transportation.gov/resource/b37r-yiaq.json',
  
  # Review Sections
  
  resub = 'https://datahub.transportation.gov/resource/yq8j-yqf2.json',
  new_england = 'https://datahub.transportation.gov/resource/yed4-dz8a.json',
  heartland = 'https://datahub.transportation.gov/resource/pu8w-cqik.json',
  gulf = 'https://datahub.transportation.gov/resource/rf6n-m9pz.json',
  appalachia = 'https://datahub.transportation.gov/resource/j6bh-426b.json',
  mid_atlantic = 'https://datahub.transportation.gov/resource/f674-dyn6.json',
  lakes = 'https://datahub.transportation.gov/resource/xq7x-rndy.json',
  desert = 'https://datahub.transportation.gov/resource/gwx5-yka7.json',
  islands = 'https://datahub.transportation.gov/resource/imwx-p856.json',
  west = 'https://datahub.transportation.gov/resource/pdrm-udaf.json',
  badlands = 'https://datahub.transportation.gov/resource/gve6-cr9a.json'
)


# State Labels & Codes
gState_Labels <- fread('resources/dat/state_labels.csv')

# Sample Sections
rss = read.socrata(url=urls['rss'], email=email, password=password)
setDT(rss)
rss = rss[state_code == 18 & year_record == 2019]
rss[, .N]

# Load Review Sections

which_url = 'lakes'

message('Downloading ', which_url)
dt = read.socrata(url=paste0(urls[which_url], '?state_code=18'), email=email, password=password)
setDT(dt)

if ( names(urls)[i] == 'mid_atlantic' ){
  dt[, value_date := ymd_hms(value_date)]
}

# Queries below --------------------------------------------------------
dt[state_code == 18 & year_record == 2019, .N, data_item]
curvesA = dt[state_code == 18 & year_record == 2019 & data_item == 'CURVES_A']
curvesA[, .N]

setdiff(curvesA[, route_id], rss[, route_id])
length(setdiff(rss[, route_id], curvesA[, route_id]))
length(intersect(curvesA[, route_id], rss[, route_id]))
length(unique(curvesA[, route_id]))

curvesA[order(route_id)][1:30]
rss[order(route_id)][1:30]
