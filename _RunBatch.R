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
# FIXME: Use named arguments with argparse package
# states, year_selection, year_compare, reimport

cargs = commandArgs(trailingOnly = TRUE)

if (!interactive() & (length(cargs) < 1) ){
  stop('Please supply a comma-delimited list of states or specify "ALL"\nFor example: Rscript RunBatch.R PA,NY,NH,VT', call.=FALSE)
} else {
  states <- cargs[1]
  year_selection <- cargs[2]
  year_compare <- cargs[3]
  reimport <- cargs[4]
}

reimport = TRUE
year_selection = 2020
year_compare = 2019
states = 'DE' #c('VT','CA')

submission_deadline <- paste(year_selection, '-06-15', sep="")

message(
  'states: ', states, 
  ' year_selection: ', year_selection, 
  ' year_compare: ', year_compare, 
  ' reimport: ', reimport
)

root <- find_root(is_git_root)
setwd(root)

dir.create('output', showWarnings = FALSE)

if ( !interactive() ){
  msg_file <- file.path(
    'output', 
    paste0('_RunBatch_messages_', format(Sys.time(), '%Y%m%d_%H%M%S.txt'))
  )

  message('Saving messages to', msg_file)

  file_con <- file(msg_file, open='wt')
  sink(file=file_con, append=FALSE, type='message')
}

message('_RunBatch.R started at ', Sys.time())
message('submission_deadline: ', submission_deadline)
message('year_selection: ', year_selection)
message('year_compare: ', year_compare)

cat('Checking availability of states for', year_selection, '\n')

con <- connect_to_db()

query <- paste(
  "select distinct stateid, datayear from", 
  sections_table,
  "order by stateid, datayear"
)

st_yr_table <- data.table(sqlQuery(con, query))

odbcClose(con)

avail_states <- st_yr_table[datayear == year_selection]$stateid

if ( length(states) == 1 && str_detect(tolower(states), 'all')){
  
  if ( length(avail_states) == 0 ) warning('No states available for ', year_selection, '\n')
  stateids <- avail_states
  state_abbrev <- getStateAbbrFromNum(stateids)
  
} else {
  
  if ( length(states) == 1 ){
    
    state_abbrev <- str_split(states, ',')[[1]]  
  }
  
  if ( length(states) > 1){
    state_abbrev <- str_replace(states, ',', '')
    state_abbrev <- str_trim(state_abbrev, side='both')
  }
  
  # Check to make sure all states are available
  
  stateids <- getStateNumFromCode(state_abbrev)
  
  which_na <- stateids[!stateids %in% avail_states] %>% getStateAbbrFromNum()
  
  if ( length(which_na) > 0 ){
    warning('States not available in the database: ', paste(which_na, collapse=', '), '\n',
            call. = FALSE, immediate. = FALSE)
  }
  
  stateids <- stateids[stateids %in% avail_states]
  state_abbrev <- getStateAbbrFromNum(stateids)
}

message('Running states:', state_abbrev, '\n\n')


# Create PDF ----------------------------------------------------------------
savepath <- "output/"

for(state in state_abbrev){
  
  msg <- paste('Started scorecard for', state, 'at', format(Sys.time(), '%H:%M:%S'))
  message(msg)
  
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
      
      success <- ImportData(
        state_selection=state,
        year_selection=year_selection
      )
      
      # Get year compare data if it hasn't been imported yet
      goverwrite = 'ALL N'
      success <- ImportData(
        state_selection=state,
        year_selection=year_compare
      )

      message('Finished importing ', state, ' at ', format(Sys.time(), '%H:%M:%S\n'))
      
      data.list <- getStateDataSets(state, year_selection, year_compare)

      create_pdf(
        data = data.list[["dat"]],
        state = data.list[["stateid"]],
        year = data.list[["year_selection"]],
        year_compare = data.list[["year_compare"]],
        # population = population,
        # national = national,
        path = savepath
      )
      
    }, error = function(cond){
      message('Error! ', conditionMessage(cond), '\n\nScorecard for ', state, ' not made\n')
      if ( !is.null(dev.list()) ) dev.off()
    } # end error
    
  )  # end tryCatch
  msg <- paste('Finished', state, 'at',  format(Sys.time(), '%H:%M:%S\n'))
  message(msg)
  message('===================================================================')
}

#warnings()
if (!interactive()){
  sink(type='message')
  close(file_con)

  message('RunBatch finished.  Check log file for warnings and errors at\n', msg_file, '\n')
} 

message('_RunBatch.R finished at ', format(Sys.time(), '%H:%M:%S\n'))

