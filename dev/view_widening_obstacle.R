# view_variable.R

# 13 Apr 2020.
# matt.landis@rsginc.com

# view a particular variable

library(dplyr)
library(data.table)
library(janitor)
library(stringr)
library(ggplot2)
library(forcats)

# load data
dirname = file.path('data', '+National', year)


# valuetext ===========================================================

# Which variables have valid values of valuetext?

# Get a list of variables
vars = dirname %>%
  file.path('*.rds') %>%
  Sys.glob() %>%
  basename() %>%
  str_replace('.rds', '')

dt_list = list()

for (variable in vars){
  
  message('-- ', variable, ' --\n')
  
  path = file.path(dirname, str_c(variable, '.rds'))
  dt = readRDS(path)
  dt_list[[variable]] = 
    dt[, .(nona_pct_numeric = sum(!is.na(valuenumeric)) / .N,
           nona_pct_text = sum(!is.na(valuetext) & valuetext != '') / .N,
           nona_pct_date = sum(!is.na(value_date)) / .N), stateid]
}

dt_nas = rbindlist(dt_list, idcol='variable')
dt_nas_m = melt(dt_nas,
                id.vars = c('variable', 'stateid'),
                value.name = 'nona_pct',
                variable.name = 'value_type')
dt_nas_m[, value_type := str_replace(value_type, 'nona_pct_', '')]

dt_nas_m1 = dt_nas_m[, .(n_states = sum(nona_pct > 0)), .(variable, value_type)]

dt_nas_m1[value_type != 'numeric'] %>%
  ggplot(aes(x = value_type, y = n_states)) +
  geom_col() +
  facet_wrap(~ variable)

# Widening obstacle =====================================================
year = 2018
variable = 'WIDENING_OBSTACLE'

path = file.path(dirname, str_c(variable, '.rds'))

dt = readRDS(path)

dt[, .N, valuetext]

# How many combinations of widening obstacle are there typically?
dt[, .N, .(valuetext, stateid)
   ][, .(n_codes = .N), stateid] %>%
  ggplot(aes(x = n_codes)) +
  geom_histogram(color='white', binwidth=10) +
  labs(x = 'N combinations',
       y = 'Number of States',
       title = 'Number of Widening Obstacle combinations per state') +
  theme_minimal()

st_code = 47
dt[stateid == st_code, .N, valuetext]


# How many unique combinations (need to sort all of them)

str = dt[stateid == st_code, valuetext]

# sort an individual string:
sort_string = function(x){
  str_c(str_sort(x), collapse='')
}

wo_combo = dt[stateid == st_code, valuetext] %>%
  # str_replace_all('(?<=[A-Z]*) ', '') %>%
  str_replace_all('[ ]', '') %>%
  str_split('') %>%
  vapply(sort_string, '')

wo_combo_dt = tabyl(wo_combo) %>%
  data.table()
wo_combo_dt[order(-n), .(wo_combo, n)]


wo_combo_dt[order(-n)][1:10] %>%
  ggplot(aes(x = fct_reorder(wo_combo, n, .desc=TRUE), y = n)) +
  geom_col() +
  labs(x = 'Widening Obstacle combination',
       y = 'Number of segments',
       title = 'Widening Obstacle, top 10 combinations',
       subtitle = str_glue('State code = {st_code}')) +
  theme_minimal()


# Frequency of letters regardless of combination
wo_code = dt[stateid == st_code, valuetext] %>%
  str_replace_all(' ', '') %>%
  str_split('') %>%
  unlist()

tabyl(wo_code) %>%
  as.data.frame() %>%
  ggplot(aes(x = wo_code, y = n)) +
  geom_col() +
  labs(x = 'Widening Obstacle code (any combination)',
       y = 'Number of segments',
       title = 'Widening Obstacle Letter Codes',
       subtitle = str_glue('State code = {st_code}')) +
  theme_minimal()

