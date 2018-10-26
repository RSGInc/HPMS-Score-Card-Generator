# RunState.R  
# Author: Matt Landis
# Date: Oct 2018
# Description: 

# Creates a scorecard for a single state.  Designed to be called from 
# RunBatch.cmd

# Setup ---------------------------------------------------------------
library('stringr')
options(warn=1)

scriptname <- 'runState.R:'

# Read args from the command line ---------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

if ( length(args) != 1 ){
  stop('This script can run only one state.  Arguments given: ',
       paste(args, collapse=', '), call.=FALSE)
}


state <- args
year_selection <- 2017
year_compare <- 2016

msg <- paste0('\n\n=========================================================\n\n',
             scriptname,
             ' Started scorecard for ', state, ' at ', format(Sys.time(), '%H:%M:%S'))
message(msg)

setwd('..')

if ( !dir.exists('output') ){
  dir.create('output')
}


# Load Code -------------------------------------------------------------------
invisible(sapply(X = list.files(path = "code", pattern = "*.R$",
                                full.names = TRUE)[-1], FUN = source))

# Increase the memory limit.  If above physical ram it will use virtual memory
invisible(memory.limit(32768))

# Create PDF ----------------------------------------------------------------
savepath <- "output/"
national <- NULL


tryCatch(
  expr = {

    # Import data ---------------------------------------------------------------

    # Overwrite current year data every time, but not comparison year data.
    # goverwrite <- 'ALL N'  # For testing - makes it go quicker.
    goverwrite <- 'ALL Y'

    success <- ImportData(state_selection=state,
                          year_selection=year_selection)

    goverwrite <- 'ALL N'
    success <- ImportData(state_selection=state,
                          year_selection=year_compare)
    cat('complete!\n')
    message('Finished importing ', state, ' at ', format(Sys.time(), '%H:%M:%S\n'))

    data.list <- getStateDataSets(state, year_selection, year_compare)

    create_pdf(data = data.list[["dat"]],
               state = data.list[["state_code"]],
               year = data.list[["year_selection"]],
               year_compare = data.list[["year_compare"]],
               # population = population,
               national = national,
               path = savepath)

  }, error = function(cond){
    message('Error! ', conditionMessage(cond), '\n\nScorecard for ', state, ' not completed\n')
    if ( !is.null(dev.list()) ) dev.off()
  } # end error

)  # end tryCatch

msg <- paste(scriptname, 'Finished scorecard for', state, 'at',  format(Sys.time(), '%H:%M:%S\n'))
message(msg)

#warnings()

