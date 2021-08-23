###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Keller and Jeff Dumont
#
#
# Description:
#
# This function creates table objects for the review pages of the score
# card.
#
###########################################################################

create_table <- function(result) {
  
  core_fontsize <- 5.15
  col_fontsize <- 4.8
  
  find_cell <- function(table, row, col, name="core-fg"){
    l <- table$layout
    which(l$t==row & l$l==col & l$name==name)
  }
  
  thm <- ttheme_default(
    core    = list(fg_params=list(col=gColors$text, fontsize=core_fontsize, hjust=1, x=0.95),
                   bg_params=list(fill='grey95'),
                   padding=unit(c(0.1, 0.1), 'inches')),
    colhead = list(fg_params=list(col=gColors$text, fontsize=col_fontsize,
                                  fontface='bold', hjust=1, x=0.95),
                   bg_params=list(fill=gColors$text_background),
                   padding=unit(c(0.1, 0.1), 'inches'))
  )
  
  if(is.null(result)) {
    return(textGrob(NoDataString, gp = gpar(fontsize = 8, col=gColors$highlight)))
  } else {
    # convert result object into tabular format
    tab <- dcast(data = result, formula = groupCat ~ variable)
    mat <- as.matrix(tab[, -c(1)])
    
    # convert to tableGrob
    myTable <- tableGrob(mat, theme=thm) 
    
    # Highlight cells with red text
    highlight <- matrix(result[, highlight], nrow=nrow(mat), ncol=ncol(mat))
    rc <- which(highlight == 1, arr.ind=TRUE)    
    
    for ( i in seq_len(dim(rc)[1] ) ){
      
      # find index of layout
      idx <- find_cell(myTable, row=rc[i, 'row'] + 1, col=rc[i, 'col'])
      
      pars <- myTable$grobs[idx][[1]][['gp']]
      pars$col <- gColors$highlight
      myTable$grobs[idx][[1]][['gp']] <- pars
      
    }
    
    # Adjust vertical alignment
    myTable <- vertically_align(myTable)
    return(myTable)
  }
}
