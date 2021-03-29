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


plotCompleteness = function(score, x, y){
  
  
  if ( is.na(score) ){
    
    border_col = 'white'
    fill_col = 'gray90'
    
  } else {
    border_col = 'slategray'
    
    fill_col = switch(as.character(score),
      `1` = 'white',      # not submitted
      `2` = 'gray75',     # submitted and incomplete
      `3` = 'slategray')  # submitted and complete
  }
  # Plot circles ----------------------------------------------------------
  
    grid.circle(
      x=x,
      y=y,
      r=unit(0.007,"npc"),
      gp=gpar(fill=fill_col, col=border_col))
}
