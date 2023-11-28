# view_variable.R

# 17 Apr 2020

# The minimum code necessary to view a particular variable
# for developing graphic displays

# Load Code
codefiles = c(Sys.glob('app/*.R'), Sys.glob('functions/*.R'))
invisible(sapply(X =codefiles , FUN = source))


view_variable = function(data.list, variable = NULL, var.label = NULL){
  
  if ( is.null(variable) ){
    variable = gVariables[Label %like% var.label, Name]
    if ( length(variable) > 1 ){
      stop('Multiple variables found with that label: ', 
           paste(variable, collapse=', '))
    }
    
  }
  
  browser()
  x1 = which(gVariables[, Name] == variable)
  create_page_summary(data = data.list$dat,
                      state = data.list$stateid,
                      year = data.list$year_selection,
                      year_compare = data.list$year_compare,
                      x1 = x1,
                      title='Test', 
                      icontext='p',
                      page=1,
                      ramps=FALSE)
}


# Set parameters

year_selection=2022
year_compare = 2020
state = 'DE'


# Load data

data.list = getStateDataSets(state, year_selection, year_compare)

debug(create_page_summary)
debug(create_traffic_detailed_review)
view_variable(data.list, variable = 'TURN_LANES_L', var.label='LEFT TURN LANES')


