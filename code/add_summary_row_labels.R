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

add_summary_row_labels <- function(year,yearcompare)
{
     grid.text(paste0(year," Summary\nStatistics\n"), x = 0.12, y = 0.805, just = "right", gp = gpar(col = "slategray", fontface = "bold", fontsize = 10))
     grid.text("Current year summaries\nby Functional System", x = 0.12, y = 0.76, just = "right", gp = gpar(col = "slategray", fontface = "italic", fontsize = 8))
     grid.text("Distributions", x = 0.12, y = 0.53, just = "right", gp = gpar(col = "slategray", fontface = "bold", fontsize = 10))
     grid.text("Plotting data distributions for\ncurrent year, previous year,\nand previous nation", x = 0.12, y = 0.48, just = "right", gp = gpar(col = "slategray", fontface = "italic", fontsize = 8))
     
     grid.text("Relative Changes\nat the Section Level", x = 0.12, y = 0.15, just = "right", gp = gpar(col = "slategray", fontface = "bold", fontsize = 10))
     grid.text("Summarize how data\nchanged year over year\nat the section level", x = 0.12, y = 0.085, just = "right", gp = gpar(col = "slategray", fontface = "italic", fontsize = 8))

     # legend for the density plots
     grid.draw(linesGrob(x = c(0.09, 0.12),
                         y = c(0.43,0.43),
                         gp=gpar(col="slategray",lwd=3)))     
     
     grid.draw(linesGrob(x = c(0.09, 0.12),
                         y = c(0.41,0.41),
                         gp=gpar(col="gray75",lwd=3)))    
     
     grid.draw(linesGrob(x = c(0.09, 0.12),
                         y = c(0.39,0.39),
                         gp=gpar(col="black",lwd=3)))    
     
     grid.text(year,x=0.08,y=0.43,hjust=1,gp = gpar(col = "slategray", fontsize = 6))
     grid.text(yearcompare,x=0.08,y=0.41,hjust=1,gp = gpar(col = "slategray", fontsize = 6))
     grid.text("National",x=0.08,y=0.39,hjust=1,gp = gpar(col = "slategray", fontsize = 6))
}
