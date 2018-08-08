###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: August 2018
# Author: Matt Landis
#
#
# Description:
#
# Creates a page to display the results of the cross-validation tests
# 
###########################################################################

create_cross_validation_page <- function(dt_cross, state, year){
  color <- 'white'

  setnames(dt_cross, old = '.id', new = 'ID')
  dt_cross <- dt_cross[order(as.numeric(ID))]
  
  grid.arrange(
    # header
    rectGrob(gp = gpar(fill = color, col = color)), # saves space for the header
    
    rectGrob(gp = gpar(fill = "slategray", col = "white")), 
    
    nrow=3, heights = unit(c(0.6, 0.03, 7.5-0.63), units="inches")
  )
  
  add_header(state, year, "cross-validation scores","cv")
  
  rowHeight <- 0.035
  colWidth <- 0.5
  x1 <- 0.08
  x2 <- x1 + 0.32
  x3 <- x2 + 0.01
  starty <- 0.75
  
  C <- 1
  R <- 1
  maxrows <- 20
  
  #browser()
  
  # grid.draw(textGrob(
  #   label='ID',
  #   x=x1 + (C - 1) * colWidth,
  #   y=starty + rowHeight,
  #   hjust = 1,
  #   gp = gpar(col = "black", fontsize = 10)
  # ))
  # 
  grid.draw(textGrob(
    label='Description',
    x=x2 + (C - 1) * colWidth,
    y=starty + rowHeight,
    hjust = 1,
    gp = gpar(col = "black", fontsize = 10)
  ))
  
  grid.draw(textGrob(
    label='Score',
    x=x3 + (C - 1) * colWidth,
    y=starty + rowHeight,
    hjust = 0,
    gp = gpar(col = "black", fontsize = 10)
  ))
  
  for (i in 1:nrow(dt_cross)){
  
    id <- dt_cross[i, ID]
    label <- dt_cross[i, Description]
    score <- round(dt_cross[i, mileage_pass] * 100 )
    
    # grid.draw(textGrob(
    #   label=id,
    #   x=x1 + (C - 1) * colWidth,
    #   y=starty - (R - 1) * rowHeight,
    #   hjust = 1,
    #   gp = gpar(col = "slategray", fontsize = 8)
    # ))
    # 
    grid.draw(textGrob(
      label=label,
      x=x2 + (C - 1) * colWidth,
      y=starty - (R - 1) * rowHeight,
      hjust = 1,
      gp = gpar(col = "slategray", fontsize = 8)
    ))
    
    plotQuality(score,
                x = x3 + (C - 1) * colWidth,
                y = starty - (R - 1) * rowHeight)
    
    if ( R <= maxrows ){
      R <- R + 1
    } else {
      R <- 1
      C <- C + 1
      # grid.draw(textGrob(
      #   label='ID',
      #   x=x1 + (C - 1) * colWidth,
      #   y=starty + rowHeight,
      #   hjust = 1,
      #   gp = gpar(col = "black", fontsize = 10)
      # ))
      # 
      grid.draw(textGrob(
        label='Description',
        x=x2 + (C - 1) * colWidth,
        y=starty + rowHeight,
        hjust = 1,
        gp = gpar(col = "black", fontsize = 10)
      ))
      
      grid.draw(textGrob(
        label='Score',
        x=x3 + (C - 1) * colWidth,
        y=starty + rowHeight,
        hjust = 0,
        gp = gpar(col = "black", fontsize = 10)
      ))
      
    }
  }
  #browser()
}