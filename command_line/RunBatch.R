#! Rscript
# RunBatch.R  
# Author: Matt Landis
# Date: July 2017
# Description: 

# This is a command line tool designed to import data from the HPMS database and
# create scorecards for a comma-delimited list of states.  The command should
# automatically overwrite existing data files and scorecards, and should not
# stop if an error for one state is encountered.


# Setup ---------------------------------------------------------------
library('stringr')
options(warn=1)

year_selection <- 2016
year_compare <- 2015

setwd('..')
msg_file <- file.path('output', '_RunBatch_messages.txt')
cat('Saving messages to', msg_file, '\n')

file_con <- file(msg_file, open='wt')
sink(file=file_con, append=FALSE, type='message')

message('RunBatch.R started at ', Sys.time())


# Read args from the command line ---------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

if ( length(args) < 1 ){
  stop('Please supply a comma-delimited list of states\nFor example: Rscript RunBatch.R PA,NY,NH,VT', call.=FALSE)
}

if ( length(args) == 1){
  state_abbrev <- str_split(args, ',')[[1]]
}

if ( length(args) > 1){
  state_abbrev <- str_replace(args, ',', '')
  state_abbrev <- str_trim(state_abbrev, side='both')
}

cat('Running states:', state_abbrev, '\n\n')


# Load Code -------------------------------------------------------------------
invisible(sapply(X = list.files(path = "code", pattern = "*.R$",
                                full.names = TRUE)[-1], FUN = source))

# Check to make sure all states are available

state_codes <- getStateNumFromCode(state_abbrev)

con <- odbcConnect("HPMS")

query <- paste("select distinct state_code, year_record from", sections_table,
               "order by state_code, year_record")

st_yr_table <- data.table(sqlQuery(con, query))

odbcClose(con)

avail_states <- st_yr_table[year_record == year_selection]$state_code

which_na <- state_codes[!state_codes %in% avail_states] %>% getStateAbbrFromNum()

if ( length(which_na) > 0 ){
  warning('States not available in the database: ', paste(which_na, collapse=', '), '\n',
          call. = FALSE, immediate. = FALSE)
}

state_codes <- state_codes[state_codes %in% avail_states]
state_abbrev <- getStateAbbrFromNum(state_codes)

# Import data ---------------------------------------------------------------

# Overwrite 2016 data every time, but not 2015 data.
# goverwrite <- 'ALL Y'
cat('Importing data from the database...')
goverwrite <- 'ALL N'
success <- ImportData(state_selection=getStateAbbrFromNum(state_codes),
           year_selection=year_selection)

goverwrite <- 'ALL N'
success <- ImportData(state_selection=getStateAbbrFromNum(state_codes),
           year_selection=year_compare)
cat('complete!\n')

# Create PDF ----------------------------------------------------------------
savepath <- "output/"
national <- NULL

for(state in state_abbrev){
  
  cat('Printing scorecard for', state, '\n')
  
  gc()
  tryCatch(
    expr = {

      data.list <- getStateDataSets(state, year_selection, year_compare)

      create_pdf(data = data.list[["dat"]],
                 state = data.list[["state_code"]],
                 year = data.list[["year_selection"]],
                 year_compare = data.list[["year_compare"]],
                 # population = population,
                 national = national,
                 path = savepath)
      
    }, error = function(cond){
      message('Error! ', conditionMessage(cond), '\n\nScorecard for ', state, ' not made\n')
      if ( !is.null(dev.list()) ) dev.off()
    } # end error
  )  # end tryCatch
  
}

#warnings()
sink(type='message')
close(file_con)


