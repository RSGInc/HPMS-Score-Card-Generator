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


plotCompleteness = function(score, x, y, item_required = 1, col_na = gColors$text_background){
  
  col_blank = gColors$blank
  col_not_submitted = gColors$blank
  col_incomplete = gColors$light
  col_complete = gColors$dark
  col_highlight = gColors$highlight
  
  if ( is.na(score) | item_required == 0 ){
    
    border_col = col_na
    fill_col = col_blank
    symbol = 21
    symbol_size = unit(3, 'mm')
    
  } else {
    
    border_col = switch(as.character(score),
                        `1` = col_highlight,
                        `2` = col_complete,
                        `3` = col_complete)
    
    fill_col = switch(as.character(score),
                      `1` = col_not_submitted,      # not submitted
                      `2` = col_incomplete,     # submitted and incomplete
                      `3` = col_complete)  # submitted and complete
    
    symbol = switch(as.character(score),
                    `1` = 13,     
                    `2` = 21,            # '\u25DO', # half circle
                    `3` = 21)
    
    symbol_size = switch(as.character(score),
                         `1` = unit(3, 'mm'),
                         `2` = unit(3, 'mm'),
                         `3` = unit(3, 'mm'))
  }

  # Plot circles ----------------------------------------------------------
  
  grid.points(
    x=x,
    y=y,
    pch=symbol,
    size=symbol_size,
    gp=gpar(fill=fill_col, col=border_col),
    default.units='npc'
  )
  
  # if ( item_required == 0 ){
  #   grid.points(
  #     x=x,
  #     y=y,
  #     pch=13,
  #     size=unit(0.01, 'npc'),
  #     gp=gpar(col=gColors$light),
  #     default.units='npc')
  # }
}
