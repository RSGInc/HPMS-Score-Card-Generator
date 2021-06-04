###########################################################################
# Title: FHWA HPMS Score Card Generator
# Date: March 2021
# Author: Matt Landis
#
#
# Description:
#
# Function to plot completeness (aka coverage) on title page. Completeness is
# calculated in calc_completeness function.
###########################################################################


plotCompleteness = function(x, y, score, item_required = 1, col_na = 'gray90'){
  
  col_blank = 'white'
  col_not_submitted = 'white'
  col_incomplete = 'gray75'
  col_complete = 'slategray'
  col_highlight = 'red'
  
  if ( is.na(score) | item_required == 0 ){
    
    border_col = col_na
    fill_col = col_blank
    
  } else {
    
    border_col = switch(as.character(score),
                        `1` = col_highlight,
                        `2` = col_complete,
                        `3` = col_complete)
    
    fill_col = switch(as.character(score),
                      `1` = col_not_submitted,      # not submitted
                      `2` = col_incomplete,     # submitted and incomplete
                      `3` = col_complete)  # submitted and complete
  }

  # Plot circles ----------------------------------------------------------
  
  grid.points(
    x=x,
    y=y,
    pch=21,
    size=unit(0.01, 'npc'),
    gp=gpar(fill=fill_col, col=border_col),
    default.units='npc'
  )
  
  # if ( item_required == 0 ){
  #   grid.points(
  #     x=x,
  #     y=y,
  #     pch=13,
  #     size=unit(0.01, 'npc'),
  #     gp=gpar(col='gray75'),
  #     default.units='npc')
  # }
}
