#! Rscript
# getStates.R  
# Author: Matt Landis
# Date: 23 Oct 2018
# Description: 

# Designed to be called from RunBatch.cmd.  Writes a file of available states

# Setup ---------------------------------------------------------------
library('stringr')
options(warn=1)

scriptname <- 'getStates.R:'
# Read args from the command line ---------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

if ( length(args) < 1 ){
  stop('Please supply a comma-delimited list of states or specify "ALL"\nFor example: Rscript RunBatch.R PA,NY,NH,VT', call.=FALSE)
}

year_selection <- 2017
year_compare <- 2016
cat(scriptname, 'started\n')

setwd('..')
args <- str_split(args, ',', simplify=TRUE)
args <- str_trim(as.vector(args), side='both')
args <- str_replace(args, '"', '')

# Load Code -------------------------------------------------------------------
invisible(sapply(X = list.files(path = "code", pattern = "*.R$",
                                full.names = TRUE)[-1], FUN = source))

# Increase the memory limit.  If above physical ram it will use virtual memory
# invisible(memory.limit(32768))

cat(scriptname, 'Checking availability of states for', year_selection, '\n')

con <- odbcConnect("HPMS")

query <- paste("select distinct state_code, year_record from", sections_table,
               "order by state_code, year_record")

st_yr_table <- data.table(sqlQuery(con, query))

odbcClose(con)

avail_states <- st_yr_table[year_record == year_selection]$state_code


if ( length(args) == 1 && str_detect(tolower(args), 'all')){
  
  if ( length(avail_states) == 0 ) warning('No states available for ', year_selection, '\n')
  state_codes <- avail_states
  state_abbrev <- getStateAbbrFromNum(state_codes)
  
} else {
  
  if ( length(args) == 1 ){
    
    state_abbrev <- str_split(args, ',')[[1]]  
  }
  
  if ( length(args) > 1){
    state_abbrev <- str_replace(args, ',', '')
    state_abbrev <- str_trim(state_abbrev, side='both')
  }
  
  # Check to make sure all states are available
  
  state_codes <- getStateNumFromCode(state_abbrev)
  
  
  which_na <- state_codes[!state_codes %in% avail_states] %>% getStateAbbrFromNum()
  
  if ( length(which_na) > 0 ){
    warning('States not available in the database: ', paste(which_na, collapse=', '), '\n',
            call. = FALSE, immediate. = FALSE)
  }
  
  state_codes <- state_codes[state_codes %in% avail_states]
  state_abbrev <- getStateAbbrFromNum(state_codes)
}

write.table(data.frame(state = state_abbrev), 'command_line/do_states.csv',
            row.names=FALSE, col.names=FALSE, sep=',')

cat(scriptname, 'Wrote file to: command_line/do_states.csv\n\n\n')
