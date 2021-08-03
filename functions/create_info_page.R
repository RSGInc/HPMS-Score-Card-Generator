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


create_info_page <- function(state, year, color=gColors$blank){
    grid.arrange(
          # header
          rectGrob(gp = gpar(fill = color, col = color)), # saves space for the header
          
          rectGrob(gp = gpar(fill = gColors$dark, col = gColors$blank)), 

          # this text gets overlayed by Kevin's image
          textGrob("information goes here!",gp = gpar(fill = gColors$blank, col = gColors$highlight)), 
          
          nrow=3,heights = unit(c(0.6,0.03,7.5-0.63),units="inches"))
     
    add_header(state,year,"how to interpret key scorecard charts","?")

    grid.raster(image=gInfoPage,x = 0, y=0.9,hjust = 0,vjust=1)
    
    gPageNumber <<- gPageNumber + 1
    add_page_number(gPageNumber)
}