# view_variable.R

# 17 Apr 2020

# The minimum code necessary to view a particular variable
# for developing graphic displays

# Load Code -------------------------------------------------------------------
invisible(sapply(X = list.files(path = "code", pattern = "*.R$",
                                full.names = TRUE)[-1], FUN = source))

view_variable = function(data.list, variable = NULL, var.label = NULL){
  
  if ( is.null(variable) ){
    variable = gVariables[Label %like% var.label, Name]
    if ( length(variable) > 1 ){
      stop('Multiple variables found with that label: ', 
           paste(variable, collapse=', '))
    }
    
  }
  
  
  x1 = which(gVariables[, Name] == variable)
  create_page_summary(data = data.list$dat,
                      state = data.list$state_code,
                      year = data.list$year_selection,
                      year_compare = data.list$year_compare,
                      x1 = x1,
                      title='Test', 
                      icontext='p',
                      page=1,
                      ramps=FALSE)
}

year_selection=2019
year_compare = 2018

state = 'NM'
data.list = getStateDataSets(state, year_selection, year_compare)
view_variable(data.list, var.label='Through Lanes')

state = 'DC'
data.list = getStateDataSets(state, year_selection, year_compare)
view_variable(data.list, var.label='Faulting')
view_variable(data.list, variable='CAPACITY')

state = 'MT'
data.list = getStateDataSets(state, year_selection, year_compare)
view_variable(data.list, var.label = 'Median Width')
view_variable(data.list, var.label = 'Cracking Percent')

state = 'PA'  
data.list <- getStateDataSets(state, year_selection, year_compare)

view_variable(data.list, variable = 'WIDENING_OBSTACLE')


state = 'MA'  
data.list <- getStateDataSets(state, year_selection, year_compare)

view_variable(data.list, variable = 'YEAR_LAST_CONSTRUCTION')
view_variable(data.list, variable = 'YEAR_LAST_IMPROV')

state = 'WY'  # CO, MN, WY

data.list <- getStateDataSets(state, year_selection, year_compare)

view_variable(data.list, variable = 'SURFACE_TYPE')
view_variable(data.list, variable = 'FUTURE_AADT')
view_variable(data.list, var.label = 'Lane Width')
view_variable(data.list, var.label = 'Through Lanes')


state = 'CO'
data.list <- getStateDataSets(state, year_selection, year_compare)

view_variable(data.list, var.label = 'Urban Code')
view_variable(data.list, var.label = 'Managed Lanes')
view_variable(data.list, var.label = 'Number of Signalized Intersections')

# Why is this continuous density -- A: it has 11 unique values
view_variable(data.list, var.label = 'Lane Width')

# Through lanes - continuous but should be discrete?
view_variable(data.list, var.label = 'Through Lanes')

view_variable(data.list, variable = gVariables[Label == 'Maintenance & Operations', Name])