# hpms_database_import_2016.R

# 8 Jun 2017
# Matt Landis

# Import HPMS 2016 data sent to us from justin.clarke@dot.gov into a database.


# Initialize ===============================================================
library('RODBC')
library('tidyverse')
library('stringr')
library('lubridate')

#q_drive <- '//i-rsg.com/rsgshares/Projects/_Federal/FHWA/15__ HPMS_DataVisualizationSupport'
onedrive <- file.path('C:/Users/matt.landis/OneDrive - Resource Systems Group, Inc')
proj_dir <- file.path(onedrive, 'Projects', 'Fhwa_hpms')
d_dir <- file.path(proj_dir, 'data')

local_con <- 'Driver={SQL Server Native Client 11.0}; server=10548L\\SQLEXPRESS;database=HPMS2016;trusted_connection=yes;'


# Work ======================================================================

# Load 2016 Sections data
raw_dir <- file.path(d_dir, 'raw_data_2016')
infile <- file.path(raw_dir, 'HPMSSections2016_IL.txt')
lns <- read_lines(infile, n_max = 100)

# File is pipe delimited
header <- read_lines(infile, n_max=1)

## tbl <- read.table(infile, header=TRUE, sep='|', quote='', as.is=TRUE,
##                  colClasses='character', fill=FALSE, skipNul=FALSE, nrows=Inf) %>%
## as_tibble()
## tbl <- tbl %>%
##   mutate(Begin_Point = as.numeric(Begin_Point),
##          End_Point = as.numeric(End_Point),
##          Value_Numeric = as.numeric(Value_Numeric),
##          Value_Date = parse_date_time(Value_Date, orders='ymdHMS'))

tbl <- read_delim(infile, delim='|')
# View(tbl)
tail(tbl)


# List and range checks
with(tbl, table(Year_Record, useNA='always'))
with(tbl, table(State_Code, useNA='always'))
sort(unique(tbl$Route_ID))

with(tbl, summary(Begin_Point))
with(tbl, summary(End_Point))
with(tbl, summary(as.numeric(Section_Length)))
tbl2 <- mutate(tbl, section_length_o = as.numeric(Section_Length),
               Section_Length = End_Point - Begin_Point)

#qplot(x=section_length_o, y=Section_Length, data=tbl2)

filter(tbl2, is.na(section_length_o)) %>%
  select(section_length_o, Section_Length, Begin_Point, End_Point)

with(tbl, summary(Value_Numeric))
with(tbl, summary(Value_Date))

tbl <- tbl %>%
  select(Year_Record, State_Code, Route_ID, Begin_Point, End_Point, Section_Length,
         Data_Item, Value_Numeric, Value_Text, Value_Date, StateYearKey)

con <- odbcDriverConnect(connection=local_con)
tbl_name <- 'ReviewSections'
sqlSave(con, dat=tbl, tablename=tbl_name, nastring=NULL, append=TRUE,
        rownames=FALSE) 

odbcClose(con)

# Write the data to the WRJ server as well.
wrj_con <- 'Driver={SQL Server Native Client 11.0}; server=wrjsqlvdw02;database=HPMS;trusted_connection=yes;'

con <- odbcDriverConnect(connection=wrj_con)
tbl_name <- 'ReviewSections'
sqlSave(con, dat=tbl, tablename=tbl_name, nastring=NULL, append=TRUE,
        rownames=FALSE) 

odbcClose(con)



# Load the 2016 sample data -------------------------------------------------
raw_dir <- file.path(d_dir, 'Samples 2016')
infile <- file.path(raw_dir, 'Review_2016_11_AdHocSQL.csv')
lns <- read_lines(infile, n_max=100)
tbl <- read_delim(file=infile, delim='|')
names_orig <- names(tbl)
names(tbl) <- tolower(names(tbl))

# List and range checks
with(tbl, table(year_record, useNA='always'))
with(tbl, table(state_code, useNA='always'))
sort(unique(tbl$route_id))

with(tbl, summary(begin_point))
with(tbl, summary(end_point))
sample_id <- tbl$sample_id %>% sort %>% unique
head(sample_id)
tail(sample_id)
with(tbl, summary(expansion_factor))
with(tbl, table(invalid))

# var_types <- c(Year_Record='smallint',
#                State_Code='smallint',
#                Route_ID='varchar(120)',
#                Begin_Point='decimal(8,3)',
#                End_Point='decimal(8,3)',
#                Section_Length='decimal(8,3)',
#                Sample_ID='varchar(12)',
#                Expansion_Factor='decimal(8,3)',
#                Invalid='smallint',
#                StateYearKey='integer')
  
# Prepare data to write
names(tbl) <- names_orig
tbl <- tbl %>%
  mutate(Section_Length = End_Point - Begin_Point,
         StateYearKey = str_c(State_Code, Year_Record %% 100))

con <- odbcDriverConnect(connection=local_con)
sm_tbl_name <- 'Review_Sample_Sections'
sqlSave(con, dat=tbl, tablename=sm_tbl_name, nastring=NULL, append=TRUE,
        rownames=FALSE) 

odbcClose(con)

# Write the data to the WRJ server as well.
wrj_con <- 'Driver={SQL Server Native Client 11.0}; server=wrjsqlvdw02;database=HPMS;trusted_connection=yes;'

con <- odbcDriverConnect(connection=wrj_con)
sm_tbl_name <- 'Review_Sample_Sections'
sqlSave(con, dat=tbl, tablename=sm_tbl_name, nastring=NULL, append=TRUE,
        rownames=FALSE) 

odbcClose(con)


