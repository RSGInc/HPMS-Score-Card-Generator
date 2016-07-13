###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont
#
#
# Description:
#
# This function creates the information page (currently page 2) of the pdf.
#
#
###########################################################################


create_info_page <- function(state,year,color="white")
{
    grid.arrange(
          # header
          rectGrob(gp = gpar(fill = color, col = color)), # saves space for the header
          
          rectGrob(gp = gpar(fill = "slategray", col = "white")), 

          # this text gets overlayed by Kevin's image
          textGrob("information goes here!",gp = gpar(fill = "white", col = "red")), 
          
          nrow=3,heights = unit(c(0.6,0.03,7.5-0.63),units="inches"))
     
    add_header(state,year,"how to interpret key scorecard charts","?")

    grid.raster(image=gLogo3,x = 0, y=0.9,hjust = 0,vjust=1)
     
    add_page_number(1)
}