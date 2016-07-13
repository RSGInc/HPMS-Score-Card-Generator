###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont
#
#
# Description:
#
# Function to the add the text elements to pages as necessary
#
###########################################################################

add_summary_col_labels <- function(title,column)
{
     xval <- c(0.27,0.565,0.86)[column]
     
     grid.text(title, x = xval, y = 0.8675, just = "centre", gp = gpar(col = "slategray", fontface = "bold", fontsize = 10))
}
