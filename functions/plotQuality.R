###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2018
# Author: Matt Landis
#
#
# Description:
#
# Function to plot quality on title page. Quality is calculated in 
# calcQuality function.
###########################################################################

plotQuality <- function(score, x, y, text=TRUE, col_na = gColors$text_background){

  bar_height = 2.5
  asp_ratio = 2
  max_length = bar_height * asp_ratio
  
  col_blank = gColors$blank
  
  if ( is.na(score) ){
    
    border_col = col_na
    fill_col = col_blank
    text = FALSE
    
  } else {
  
    border_col = gColors$dark
    fill_col = gColors$dark
    
  }
  
  # browser()
  
  grid.rect(x=x,
            y=y,
            width=unit(max_length, "mm"),
            height=unit(bar_height, "mm"),
            just='left',
            gp=gpar(fill=col_blank, col=border_col))
  
  grid.rect(x=x,
            y=y,
            width=unit(max_length * (score / 100), "mm"),
            height=unit(bar_height, "mm"),
            just='left',
            gp=gpar(fill=fill_col, col=border_col))
  
  if ( text ){
    grid.text(label = score,
              x= unit(x, 'npc') + unit(max_length, 'mm') + unit(0.004, 'npc'),
              y=y,
              just = c(0, 0.4),
              gp = gpar(col=border_col, fontsize=6.5))
  }
  
}
