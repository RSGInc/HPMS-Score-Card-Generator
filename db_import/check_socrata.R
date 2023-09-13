# check_db.R

# A simple R script to check things in the Socrata database

library('data.table')
library('stringr')
library('lubridate')
library('RSocrata')
source('db_import/socrata_functions.R')

email = 'matt.landis@rsginc.com'
password = 'tX522b7Dz4xS'

year = 2019
this_abbrev = 'DC'



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
stateid = gState_Labels[abbr == this_abbrev, index]

# Sample Sections
rss = read.socrata(url=urls['rss'], email=email, password=password)
setDT(rss)
rss = rss[stateid == stateid & datayear == year]
rss[, .N]

# Load Review Sections

which_url = 'mid_atlantic'

message('Downloading ', which_url)
dt = read.socrata(url=paste0(urls[which_url], '?stateid=', stateid), email=email, password=password)
setDT(dt)

if ( which_url == 'mid_atlantic' ){
  dt[, value_date := ymd_hms(value_date)]
}

# Queries below --------------------------------------------------------

this_item = 'BASE_TYPE'
dt[dataitem == this_item, .N]
