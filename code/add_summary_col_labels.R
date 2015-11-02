###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2015
# Author: Jeff Dumont
#
#
# Description:
#
# Author needs to add a description!
#
###########################################################################

add_summary_col_labels <- function(title,column)
{
     xval <- c(0.27,0.565,0.86)[column]
     
     grid.text(title, x = xval, y = 0.8675, just = "centre", gp = gpar(col = "slategray", fontface = "bold", fontsize = 10))
}
