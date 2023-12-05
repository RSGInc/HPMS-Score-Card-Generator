# view_cross_validation.R

# 8 Jul 2020

# The minimum code necessary to view a particular variable
# for developing graphic displays

# Load Code -------------------------------------------------------------------
invisible(sapply(X = list.files(path = "code", pattern = "*.R$",
                                full.names = TRUE)[-1], FUN = source))


year_selection=2019
year_compare = 2018

state = 'ME'  
data.list <- getStateDataSets(state, year_selection, year_compare)


dt_cross <- calc_cross_validation(data=data.list$dat, year=data.list$year_selection)
