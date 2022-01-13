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
library('rprojroot')
library('rmarkdown')

options(warn=1)
options(scipen=9999)

# Load Code -------------------------------------------------------------------
codefiles = c(Sys.glob('app/*.R'), Sys.glob('functions/*.R'))
invisible(sapply(X =codefiles , FUN = source))

# Read args from the command line ---------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

if ( length(args) < 1 ){
  stop('Please supply a comma-delimited list of states or specify "ALL"\nFor example: Rscript RunBatch.R PA,NY,NH,VT', call.=FALSE)
}

reimport <- TRUE
year_selection <- 2020
year_compare <- 2019

if (length(args) > 0) {
  if(!str_detect(tolower(args[1]), 'all')) {
    state_abbrev <- str_split(args, ',')[[1]]
  }
}

if (length(args) > 1) {
  year_selection <- str_trim(args[2], side='both')
}

if(length(args) > 2) {
  year_compare <- str_trim(args[3], side='both')
}

if(length(args) > 3) {
  reimport <- str_trim(args[4], side='both')
}

if(length(args) > 4) {
  db_username <- str_trim(args[5], side='both')
}

if(length(args) > 5) {
  db_password <- str_trim(args[6], side='both')
}

submission_deadline <- paste(year_selection, '-06-15', sep="")
cat('state_abbrev: ', state_abbrev, ' year_selection: ', year_selection, 
' year_compare: ', year_compare, ' reimport: ', reimport, 
' db_username: ', db_username, ' db_password: ', db_password, '\n')

root <- rprojroot::find_rstudio_root_file()
setwd(root)

msg_file <- file.path('output', paste0('_RunBatch_messages_',
format(Sys.time(), '%Y%m%d_%H%M%S.txt')))

cat('Saving messages to', msg_file, '\n')

if ( !dir.exists('output') ){
  dir.create('output')
}

file_con <- file(msg_file, open='wt')
sink(file=file_con, append=FALSE, type='message')

message('RunBatch.R started at ', Sys.time())
message('submission_deadline: ', submission_deadline)
message('year_selection: ', year_selection)
message('year_compare: ', year_compare)

cat('Checking availability of states for', year_selection, '\n')

con <- GetODBCConnection()

query <- paste("select distinct state_code, year_record from", sections_table,
               "order by state_code, year_record")
st_yr_table <- data.table(sqlQuery(con, query))

odbcClose(con)

avail_states <- st_yr_table[year_record == year_selection]$state_code

if(str_detect(tolower(args[1]), 'all')) {
    if ( length(avail_states) == 0 ) warning('No states available for ', year_selection, '\n')
    state_codes <- avail_states
    state_abbrev <- getStateAbbrFromNum(state_codes)
  } else {
    # Check to make sure all states are available
    state_codes <- getStateNumFromCode(state_abbrev)
    which_na <- state_codes[!state_codes %in% avail_states] %>% getStateAbbrFromNum()
    if (length(which_na) > 0 ){
      warning('States not available in the database: ', paste(which_na, collapse=', '), '\n',
            call. = FALSE, immediate. = FALSE)
    }
    state_codes <- state_codes[state_codes %in% avail_states]
    state_abbrev <- getStateAbbrFromNum(state_codes)
  }

cat('Running states:', state_abbrev, '\n\n')

# Create PDF ----------------------------------------------------------------
savepath <- "output/"

for(state in state_abbrev){
  
  msg <- paste('Started scorecard for', state, 'at', format(Sys.time(), '%H:%M:%S'))
  cat(msg, '\n')
  message(msg)
  
  gc()
  tryCatch(
    expr = {

      # Import data ---------------------------------------------------------------
      
      # Overwrite current year data but not comparison data.
      # goverwrite <- 'ALL N'  # For testing - makes it go quicker.
      if ( reimport ){
        goverwrite = 'ALL Y'
      } else {
        goverwrite = 'ALL N'
      }
      
      success <- ImportData(state_selection=state,
                            year_selection=year_selection)
      
      goverwrite = 'ALL N'
      success <- ImportData(state_selection=state,
                            year_selection=year_compare)
      cat('Import complete!\n')
      message('Finished importing ', state, ' at ', format(Sys.time(), '%H:%M:%S\n'))
      
      data.list <- getStateDataSets(state, year_selection, year_compare)

      create_pdf(data = data.list[["dat"]],
                 state = data.list[["state_code"]],
                 year = data.list[["year_selection"]],
                 year_compare = data.list[["year_compare"]],
                 # population = population,
                 # national = national,
                 path = savepath)
      
    }, error = function(cond){
      message('Error! ', conditionMessage(cond), '\n\nScorecard for ', state, ' not made\n')
      if ( !is.null(dev.list()) ) dev.off()
    } # end error
    
  )  # end tryCatch
  msg <- paste('Finished', state, 'at',  format(Sys.time(), '%H:%M:%S\n'))
  cat(msg)    
  message(msg)
  message('===================================================================')
}

#warnings()
sink(type='message')
close(file_con)

cat('RunBatch finished.  Check log file for warnings and errors at\n', msg_file, '\n')
