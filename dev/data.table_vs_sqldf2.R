





# data replacements ---------------------------------------------------------

# Search text
# dat[.](.*) <- data\[data_item == .* & year_record == year, \]

# Replace text
# dat.$1 = data[
#   data_item == $1 & year_record == year,
#   .(route_id, begin_point, end_point, expansion_factor, $1 = value_numeric)]

dat.variable[, .(route_id, begin_point, end_point, variable = value_numeric)]

dat.FACILITY_TYPE[, .(route_id, begin_point, end_point, FACILITY_TYPE = value_numeric)]
dat.F_SYSTEM[, .(route_id, begin_point, end_point, F_SYSTEM = value_numeric)]
dat.URBAN_ID[, .(route_id, begin_point, end_point, URBAN_ID = value_numeric)]
dat.NHS[, .(route_id, begin_point, end_point, NHS = value_numeric)]

dat.THROUGH_LANES[, .(route_id, begin_point, end_point, THROUGH_LANES = value_numeric)]
dat.MEDIAN_TYPE[, .(route_id, begin_point, end_point, MEDIAN_TYPE = value_numeric)]

target = copy(coverage)

# Change for variable --------------------

coverage = dat.FACILITY_TYPE[, .(route_id, begin_point, end_point, FACILITY_TYPE = value_numeric)] %>%
  coverage_join(
    dat.variable[, .(route_id, begin_point, end_point, variable = value_numeric)]) %>%
  coverage_join(
    dat.F_SYSTEM[, .(route_id, begin_point, end_point, F_SYSTEM = value_numeric)]) %>%
  coverage_join(
    dat.NHS[, .(route_id, begin_point, end_point, NHS = value_numeric)])

coverage[, required := FACILITY_TYPE %in% c(1,2) & (F_SYSTEM %in% c(1,2,3,4)|!is.na(NHS))]

# ---------------------------------


# Compare the two versions

(keep_cols = names(coverage))
target = target[, keep_cols, with=FALSE]
setkeyv(target, key(coverage))
setcolorder(target, neworder=names(coverage))

all.equal(target, coverage)
address(target) == address(coverage)

coverage[target, i.required := i.required, on = .(route_id, begin_point, end_point)]

coverage[required != i.required, .N] / coverage[, .N] * 100

coverage[, .N, keyby = .(required, i.required)]

# Priority variables
# OWNERSHIP
# THROUGH_LANES
# URBAN_ID
# AADT
# AADT_COMBINATION
# AADT_SINGLE_UNIT
# ROUTE_NUMBER
# ROUTE_QUALIFIER
# ROUTE_SIGNING
# NHS


# FACILITY_TYPE
# THROUGH_LANES
# URBAN_ID
# F_SYSTEM
# NHS
# COUNTY_ID
# IRI
# SURFACE_TYPE
# MAINTENANCE_OPERATIONS
# TOLL_CHARGED
# ROUTE_NUMBER
# ROUTE_SIGNING
# ACCESS_CONTROL
# SHOULDER_WIDTH_L
# SHOULDER_TYPE
# MEDIAN_TYPE

