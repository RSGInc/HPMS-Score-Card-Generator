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
  color <- gColors$blank
  
  maxrows <- 21
  max_stringwidth <- 90
  desc_width <- 120 # mm
  rowHeight <- 8   # mm
  fontsize <- 7
  
  setnames(dt_cross, old = c('.id', 'Description', 'mileage_total'), new = c('ID', 'Desc_old', 'Miles'))
  dt_cross <- dt_cross[order(as.numeric(ID))]
  desc_new <- sapply(
    strwrap(dt_cross$Desc_old, width=max_stringwidth, simplify=FALSE),
    paste, collapse='\n')
  dt_cross[, Description := desc_new]
  dt_cross[, Miles := string_format(round(Miles, 1))]
  
  dt_1 <- dt_cross[1:maxrows]
  dt_2 <- dt_cross[(maxrows + 1):nrow(dt_cross)]
  
  # Make tables
  
  # Left table
  core_hjust <- matrix(c(1, 1, 1), ncol=3, nrow=nrow(dt_1), byrow=TRUE)
  core_x <- matrix(c(0.99, 0.9, 0.99), ncol=3, nrow=nrow(dt_1), byrow=TRUE)
  
  thm <- ttheme_default(
    core    = list(fg_params=list(col=gColors$dark, fontsize=fontsize, hjust=core_hjust, x=core_x),
                   padding = unit(c(0, 0), 'mm')),
    colhead = list(fg_params=list(col=gColors$text, fontsize=fontsize,
                                  fontface='bold', hjust=0.5),
                   padding=unit(c(3, 3), 'mm')))
  
  tbl_1 <- tableGrob(dt_1[, .(Description, Miles, Score = '')], rows=NULL, theme=thm,
                   heights=unit(rep(rowHeight, nrow(dt_1)), 'mm'),
                   widths=unit(c(desc_width, 15, 20), 'mm'))
  tbl_1$vp <- viewport(x=0.5, y=unit(1, 'npc') - 0.5 * sum(tbl_1$heights))
  
  # Right table
  core_hjust <- matrix(c(1, 1, 1), ncol=3, nrow=nrow(dt_2), byrow=TRUE)
  core_x <- matrix(c(0.99, 0.9, 0.99), ncol=3, nrow=nrow(dt_2), byrow=TRUE)
  
  thm <- ttheme_default(
    core    = list(fg_params=list(col=gColors$dark, fontsize=fontsize, hjust=core_hjust, x=core_x),
                   padding = unit(c(0, 0), 'mm')),
    colhead = list(fg_params=list(col=gColors$text, fontsize=fontsize,
                                  fontface='bold', hjust=0.5),
                   padding=unit(c(3, 3), 'mm')))
  
  tbl_2 <- tableGrob(dt_2[, .(Description, Miles, Score = '')], rows=NULL, theme=thm,
                     heights=unit(rep(rowHeight, nrow(dt_2)), 'mm'),
                     widths=unit(c(desc_width, 15, 20), 'mm'))
  tbl_2$vp <- viewport(x=0.5, y=unit(1, 'npc') - 0.5 * sum(tbl_2$heights))
  
  ob <- arrangeGrob(
    # header
    rectGrob(gp = gpar(fill = color, col = color)), # saves space for the header
    rectGrob(gp = gpar(fill = gColors$dark, col = gColors$blank)), 
    tbl_1, tbl_2,
    layout_matrix = rbind(c(1,1), c(2,2), c(3, 4)),
    heights = unit(c(0.6, 0.03, 7.5-0.63), units="inches")
  )
  
  grid.newpage()
  grid.draw(ob)
    
  add_header(state, year, "cross-validation scores","cv")
  
  # Add the scores
  colWidth <- 0.5
  startx <- 0.430
  starty <- 0.8675
  
  C <- 1
  R <- 1
  
  for (i in 1:nrow(dt_cross)){
  
    #id <- dt_cross[i, ID]
    #label <- dt_cross[i, Description]
    score <- round(dt_cross[i, mileage_pass] * 100 )
    
    plotQuality(score,
                x = startx + (C - 1) * colWidth,
                y = unit(starty, 'npc') - (R - 1) * unit(rowHeight, 'mm'))
    
    if ( R >= maxrows ){
      #browser()
      R <- 1
      C <- C + 1
    } else {
      R <- R + 1
    }
  }
  gPageNumber <<- gPageNumber + 1
  add_page_number(gPageNumber)
}

# End
