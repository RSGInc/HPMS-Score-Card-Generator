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
  }
  
  grob = create_travel_yoy_density(data = data.list$dat,
                                   state = data.list$state_code,
                                   year = data.list$year_selection,
                                   yearcomparison = data.list$year_compare,
                                   variable = variable,
                                   includeNational = TRUE,
                                   ramps=FALSE)
  showGrob(grob)
}

year_selection=2018
year_compare = 2017

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