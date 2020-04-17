# view_variable.R

# 17 Apr 2020

# The minimum code necessary to view a particular variable
# for developing graphic displays

# Load Code -------------------------------------------------------------------
invisible(sapply(X = list.files(path = "code", pattern = "*.R$",
                                full.names = TRUE)[-1], FUN = source))

state = 'AL'
year_selection=2018
year_compare = 2017

data.list <- getStateDataSets(state, year_selection, year_compare)

view_variable = function(data.list, variable){
  grob = create_travel_yoy_density(data = data.list$dat,
                            state = data.list$state_code,
                            year = data.list$year_selection,
                            yearcomparison = data.list$year_compare,
                            variable = variable,
                            includeNational = TRUE,
                            ramps=FALSE)
  showGrob(grob)
}

view_variable(data.list, variable = 'SURFACE_TYPE')
