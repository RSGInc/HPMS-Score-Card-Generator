###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont
#
#
# Description:
#
# Function to the add page numbers to the pdf
#
###########################################################################

add_page_number <- function(number)
{
  grid.text(paste0("page ",number),x=0.995,y=0.015,gp=gpar(fontsize=7, col=gColors$dark),hjust=1)
}