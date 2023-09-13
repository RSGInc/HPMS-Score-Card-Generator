
library(tictoc)

# Compare two different joins --------------------------------------------------

coverage <- sqldf("select 
 A.routeid,A.beginpoint,A.endpoint,A.dataitem,A.value_numeric as FACILITYTYPE, 
 B.value_numeric as variable,B.expansion_factor 
 from [dat.FACILITY_TYPE] A 
 left join [dat.variable] B on 
 A.routeid = B.routeid and (
 ( A.beginpoint between B.beginpoint and B.endpoint and A.endpoint between B.beginpoint and B.endpoint ) or
 ( B.beginpoint between A.beginpoint and A.endpoint and B.endpoint between A.beginpoint and A.endpoint )
 )")
setDT(coverage)

A = dat.FACILITY_TYPE[, .(routeid, beginpoint, endpoint, dataitem, FACILITYTYPE = value_numeric)]
B = dat.variable[, .(routeid, beginpoint, endpoint, variable = value_numeric, expansion_factor)]

setkey(A, routeid, beginpoint, endpoint)
setkey(B, routeid, beginpoint, endpoint)

join_cols = c('routeid', 'beginpoint', 'endpoint')

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

coverage2[, (c('beginpoint', 'endpoint')) := NULL]
setnames(coverage2, c('i.beginpoint', 'i.endpoint'), c('beginpoint', 'endpoint'))
setcolorder(coverage2, neworder = names(coverage))
setkeyv(coverage, cols=key(coverage2))

coverage
coverage2

# Check
test = merge(coverage, coverage2, by = c('routeid', 'beginpoint', 'endpoint'), all=TRUE)
test[dataitem.x != dataitem.y, .N]
test[FACILITYTYPE.x != FACILITYTYPE.y, .N]
test[variable.x != variable.y, .N]

test[variable.x != variable.y][order(routeid, beginpoint, endpoint),
  .(routeid, beginpoint, endpoint, variable.x, variable.y, expansion_factor.x, expansion_factor.y)]

B[routeid == '00400I00' & beginpoint >= 119 & endpoint <= 122]
all.equal(coverage, coverage2)




# sqldf
tic('sqldf')

coverage1 <- sqldf("select 
 A.routeid,A.beginpoint,A.endpoint,A.dataitem,A.value_numeric as FACILITYTYPE, 
 B.value_numeric as variable,B.expansion_factor 
 from [dat.FACILITY_TYPE] A 
 left join [dat.variable] B on 
 A.routeid = B.routeid and (
 ( A.beginpoint between B.beginpoint and B.endpoint and A.endpoint between B.beginpoint and B.endpoint ) or
 ( B.beginpoint between A.beginpoint and A.endpoint and B.endpoint between A.beginpoint and A.endpoint )
 )")
setDT(coverage1)

coverage2 <- sqldf("select 
 A.*, 
 B.value_numeric as FSYSTEM 
 from [coverage1] A 
 left join [dat.F_SYSTEM] B on 
 A.routeid = B.routeid and (
 ( A.beginpoint between B.beginpoint and B.endpoint and A.endpoint between B.beginpoint and B.endpoint ) or
 ( B.beginpoint between A.beginpoint and A.endpoint and B.endpoint between A.beginpoint and A.endpoint )
 ) ")

coverage3 <- sqldf("select 
 A.*, 
 B.value_numeric as NHS 
 from [coverage2] A 
 left join [dat.NHS] B on 
 A.routeid = B.routeid and (
 ( A.beginpoint between B.beginpoint and B.endpoint and A.endpoint between B.beginpoint and B.endpoint ) or
 ( B.beginpoint between A.beginpoint and A.endpoint and B.endpoint between A.beginpoint and A.endpoint )
 ) ")

setDT(coverage3)
setkey(coverage3, routeid, beginpoint, endpoint)

toc()  # 67.5 seconds.


# data.table
tic('data.table')

cov1 = coverage_join(
  dat.FACILITY_TYPE[, .(routeid, beginpoint, endpoint, dataitem, FACILITYTYPE = value_numeric)],
  dat.variable[, .(routeid, beginpoint, endpoint, variable = value_numeric, expansion_factor)])

cov2 = coverage_join(
  cov1,
  dat.F_SYSTEM[, .(routeid, beginpoint, endpoint, FSYSTEM = value_numeric)])

cov3 = coverage_join(
  cov2,
  dat.NHS[, .(routeid, beginpoint, endpoint, NHS = value_numeric)])

toc()


# Compare
test = merge(coverage3, cov3, all=TRUE)

test[dataitem.x != dataitem.y, .N]
test[FACILITYTYPE.x != FACILITYTYPE.y, .N]
test[variable.x != variable.y, .N]
test[expansion_factor.x != expansion_factor.y, .N]
test[FSYSTEM.x != FSYSTEM.y, .N]
test[NHS.x != NHS.y, .N]
