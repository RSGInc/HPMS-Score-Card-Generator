###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2015
# Author: Jeff Keller
#
#
# Description:
#
# This function is used to expand vectors. Used in the
# create_travel_data_yoy function.
#
###########################################################################

vecInsert <- function(x, y, idx) {
     new_x <- c(x, y)
     new_idx <- c(seq_along(x), idx - 0.5)
     new_x[order(new_idx)]
}
