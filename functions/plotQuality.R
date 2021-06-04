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

plotQuality <- function(x, y, score, text=TRUE, col_na = 'gray90'){

  full_length <- 0.015
  
  col_blank = 'white'
  
  if ( is.na(score) ){
    
    border_col = col_na
    fill_col = 'white'
    text = FALSE
    
  } else {
  
    border_col = 'slategray'
    fill_col = 'slategray'
    
  }
  
  grid.rect(x=x,
            y=y,
            width=unit(full_length, "npc"),
            height=unit(0.0125, "npc"),
            just='left',
            gp=gpar(fill=col_blank, col=border_col))
  
  grid.rect(x=x,
            y=y,
            width=unit(full_length * (score / 100), "npc"),
            height=unit(0.0125, "npc"),
            just='left',
            gp=gpar(fill=fill_col, col=border_col))
  
  if ( text ){
    grid.text(label = score,
              x= x + full_length + 0.004,
              y=y,
              just = c(0, 0.5),
              gp = gpar(col=border_col, fontsize=6.5))
  }
  
}
