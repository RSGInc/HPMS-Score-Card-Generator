
library(tictoc)

# Compare two different joins --------------------------------------------------

coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as FACILITYTYPE, 
 B.value_numeric as variable,B.expansion_factor 
 from [dat.FACILITY_TYPE] A 
 left join [dat.variable] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
setDT(coverage)

A = dat.FACILITY_TYPE[, .(route_id, begin_point, end_point, data_item, FACILITYTYPE = value_numeric)]
B = dat.variable[, .(route_id, begin_point, end_point, variable = value_numeric, expansion_factor)]

setkey(A, route_id, begin_point, end_point)
setkey(B, route_id, begin_point, end_point)

join_cols = c('route_id', 'begin_point', 'end_point')

# test_join = merge(A, B, by = join_cols, all=TRUE)
# # Test within joings
# ab = foverlaps(A, B, by.x=join_cols, by.y=join_cols, type='within', mult='last', which=TRUE)
# ba = foverlaps(B, A, by.x=join_cols, by.y=join_cols, type='within', mult='last', which=TRUE)

coverage2 = foverlaps(
  A,
  B,
  by.x = join_cols,
  by.y = join_cols,
  type='within',
  mult='last',
  nomatch=NA)

coverage2[, .N]

coverage2[, (c('begin_point', 'end_point')) := NULL]
setnames(coverage2, c('i.begin_point', 'i.end_point'), c('begin_point', 'end_point'))
setcolorder(coverage2, neworder = names(coverage))
setkeyv(coverage, cols=key(coverage2))

coverage
coverage2

# Check
test = merge(coverage, coverage2, by = c('route_id', 'begin_point', 'end_point'), all=TRUE)
test[data_item.x != data_item.y, .N]
test[FACILITYTYPE.x != FACILITYTYPE.y, .N]
test[variable.x != variable.y, .N]

test[variable.x != variable.y][order(route_id, begin_point, end_point),
  .(route_id, begin_point, end_point, variable.x, variable.y, expansion_factor.x, expansion_factor.y)]

B[route_id == '00400I00' & begin_point >= 119 & end_point <= 122]
all.equal(coverage, coverage2)




# sqldf
tic('sqldf')

coverage1 <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as FACILITYTYPE, 
 B.value_numeric as variable,B.expansion_factor 
 from [dat.FACILITY_TYPE] A 
 left join [dat.variable] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
setDT(coverage1)

coverage2 <- sqldf("select 
 A.*, 
 B.value_numeric as FSYSTEM 
 from [coverage1] A 
 left join [dat.F_SYSTEM] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")

coverage3 <- sqldf("select 
 A.*, 
 B.value_numeric as NHS 
 from [coverage2] A 
 left join [dat.NHS] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")

setDT(coverage3)
setkey(coverage3, route_id, begin_point, end_point)

toc()  # 67.5 seconds.

coverage_join = function(a, b){
  
  join_cols = c('route_id', 'begin_point', 'end_point')
  setkeyv(a, cols=join_cols)
  setkeyv(b, cols=join_cols)
  
  ab = foverlaps(
    a,
    b,
    by.x = join_cols,
    by.y = join_cols,
    type='within',
    mult='first',
    nomatch=NA)

  ab[, (c('begin_point', 'end_point')) := NULL]
  setnames(ab, c('i.begin_point', 'i.end_point'), c('begin_point', 'end_point'))
  setkeyv(ab, join_cols)
  
  # ba = foverlaps(
  #   b,
  #   a,
  #   by.x=join_cols,
  #   by.y=join_cols,
  #   type='within',
  #   mult='first',
  #   nomatch=NA)
  # 
  # ba[, (c('begin_point', 'end_point')) := NULL]
  # setnames(ba, c('i.begin_point', 'i.end_point'), c('begin_point', 'end_point'))
  # setkeyv(ba, join_cols)
  # 
  # measure_cols = setdiff(names(ba), join_cols)
  # setnames(ba, measure_cols, paste0(measure_cols, '_tmp'))
  # 
  # abba = merge(
  #   ab,
  #   ba,
  #   by = join_cols,
  #   all.x=TRUE)
  # 
  # abba[variable != variable_tmp]
  # abba[expansion_factor != expansion_factor_tmp]
  
  return(ab)
  
}

# data.table
tic('data.table')

cov1 = coverage_join(
  dat.FACILITY_TYPE[, .(route_id, begin_point, end_point, data_item, FACILITYTYPE = value_numeric)],
  dat.variable[, .(route_id, begin_point, end_point, variable = value_numeric, expansion_factor)])

cov2 = coverage_join(
  cov1,
  dat.F_SYSTEM[, .(route_id, begin_point, end_point, FSYSTEM = value_numeric)])

cov3 = coverage_join(
  cov2,
  dat.NHS[, .(route_id, begin_point, end_point, NHS = value_numeric)])

toc()


# Compare
test = merge(coverage3, cov3, all=TRUE)

test[data_item.x != data_item.y, .N]
test[FACILITYTYPE.x != FACILITYTYPE.y, .N]
test[variable.x != variable.y, .N]
test[expansion_factor.x != expansion_factor.y, .N]
test[FSYSTEM.x != FSYSTEM.y, .N]
test[NHS.x != NHS.y, .N]
