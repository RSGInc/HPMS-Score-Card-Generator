###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2015
# Author: Jeff Keller and Jeff Dumont
#
#
# Description:
#
# This function creates table objects for the second page of the score
# card.
#
###########################################################################

create_table <- function(result) {
     
     if(is.null(result)) {
          return(textGrob("No Data", gp = gpar(fontsize = 16, col = "red")))
     } else {
          # convert result object into tabular format
          tab <- dcast(data = result, formula = groupCat ~ variable)
          mat <- as.matrix(tab[, -c(1)])
          
          # pad highlight vector to skip the column headers
          highlight <- result[, highlight][match(mat, result[, value])]
          highlight <- vecInsert(x = highlight, y = rep(0, ncol(mat)), idx = 1 + 0: (ncol(mat) - 1) * nrow(mat))
          
          # convert to tableGrob
          myTable <- tableGrob(mat, 
                               core.just = "right",
                               col.just="right",
                               rows=NULL, 
                               gpar.coretext = gpar(col = "black",fontsize=5.15),
                               gpar.coltext = gpar(col = "black",fontsize=4.8, fontface = "bold"),
                               padding.h=unit(0.1,units="inches"),padding.v=unit(0.1,units="inches")
          )
          
          # Style the highlighted cells
          for (i in 1:length(highlight)) {
               if (highlight[i] == 1) {
                    myTable$lg$lgt[[i + nrow(mat) + 1]]$gp$col <- "red"
                    myTable$lg$lgt[[i + nrow(mat) + 1]]$gp$cex <- 1
               }
          }
          
          myTable <- vertically_align(myTable)
          
          return(myTable)
     }
     
}
