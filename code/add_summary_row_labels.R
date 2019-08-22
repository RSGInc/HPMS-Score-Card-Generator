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

add_summary_row_labels <- function(year,yearcompare,ramps=TRUE)
{
     grid.text(paste0(year," Summary\nStatistics\n"), x = 0.105, y = 0.805, just = "right", gp = gpar(col = "slategray", fontface = "bold", fontsize = 10))
     grid.text("Current year summaries\nby Functional System", x = 0.105, y = 0.77, just = "right", gp = gpar(col = "slategray", fontface = "italic", fontsize = 7))
     if(!ramps){
       grid.text("Quality result details in ().\no: Outliers\na: Adjacency\ny: Year-over-year", x = 0.105, y = 0.7, just = "right", gp = gpar(col = "slategray", fontface = "italic", fontsize = 7))
     }
     grid.text("Distributions", x = 0.105, y = 0.53, just = "right", gp = gpar(col = "slategray", fontface = "bold", fontsize = 10))
     grid.text("Plotting data distributions for\ncurrent year, previous year,\nand national", x = 0.105, y = 0.49, just = "right", gp = gpar(col = "slategray", fontface = "italic", fontsize = 7))
     
     grid.text("Sample panel data are unexpanded.", x = 0.105, y = 0.37, just = "right", gp = gpar(col = "slategray", fontface = "italic", fontsize = 6))
     grid.text("National is scaled to state data.", x = 0.105, y = 0.35, just = "right", gp = gpar(col = "slategray", fontface = "italic", fontsize = 6))
          
     grid.text("Relative Changes\nat the Section Level", x = 0.105, y = 0.15, just = "right", gp = gpar(col = "slategray", fontface = "bold", fontsize = 10))
     grid.text("Summarize how data\nchanged year over year\nat the section level\n*indicates where a \nlow % is expected", x = 0.105, y = 0.075, just = "right", gp = gpar(col = "slategray", fontface = "italic", fontsize = 7))

     # legend for the density plots
     grid.draw(linesGrob(x = c(0.075, 0.105),
                         y = c(0.43,0.43),
                         gp=gpar(col="slategray",lwd=3)))     
     
     grid.draw(linesGrob(x = c(0.075, 0.105),
                         y = c(0.41,0.41),
                         gp=gpar(col="gray75",lwd=3)))    
     
     grid.draw(linesGrob(x = c(0.075, 0.105),
                         y = c(0.39,0.39),
                         gp=gpar(col="black",lwd=3)))    
     
     grid.text(year,       x=0.065,y=0.43,hjust=1,gp = gpar(col = "slategray", fontsize = 6))
     grid.text(yearcompare,x=0.065,y=0.41,hjust=1,gp = gpar(col = "slategray", fontsize = 6))
     grid.text(paste0("National (",yearcompare,")"), x=0.065,y=0.39,hjust=1,gp = gpar(col = "slategray", fontsize = 6))
}
