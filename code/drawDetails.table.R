###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2015
# Author: Jeff Dumont & Jeff Keller
#
#
# Description:
#
# This function redefines the drawDetails method for tables so that
# tableGrobs can be intercepted and modified.
#
###########################################################################

drawDetails.table <- function (x, recording = TRUE) {
     lg <- if(!is.null(x$lg)) {
          x$lg
     } else {
          with(x, gridExtra:::makeTableGrobs(as.character(as.matrix(d)), 
                                             rows, cols, NROW(d), NCOL(d), parse, row.just = row.just, 
                                             col.just = col.just, core.just = core.just, equal.width = equal.width, 
                                             equal.height = equal.height, gpar.coretext = gpar.coretext, 
                                             gpar.coltext = gpar.coltext, gpar.rowtext = gpar.rowtext, 
                                             h.odd.alpha = h.odd.alpha, h.even.alpha = h.even.alpha, 
                                             v.odd.alpha = v.odd.alpha, v.even.alpha = v.even.alpha, 
                                             gpar.corefill = gpar.corefill, gpar.rowfill = gpar.rowfill, 
                                             gpar.colfill = gpar.colfill))
     }
     widthsv <- convertUnit(lg$widths + x$padding.h, "mm", valueOnly = TRUE)
     heightsv <- convertUnit(lg$heights + x$padding.v, "mm", valueOnly = TRUE)
     widthsv[1] <- widthsv[1] * as.numeric(x$show.rownames)
     widths <- unit(widthsv, "mm")
     heightsv[1] <- heightsv[1] * as.numeric(x$show.colnames)
     heights <- unit(heightsv, "mm")
     cells = viewport(name = "table.cells", layout = grid.layout(lg$nrow + 
                                                                      1, lg$ncol + 1, widths = widths, heights = heights))
     pushViewport(cells)
     tg <- gridExtra:::arrangeTableGrobs(lg$lgt, lg$lgf, lg$nrow, lg$ncol, 
                                         lg$widths, lg$heights, show.colnames = x$show.colnames, 
                                         show.rownames = x$show.rownames, padding.h = x$padding.h, 
                                         padding.v = x$padding.v, separator = x$separator, show.box = x$show.box, 
                                         show.vlines = x$show.vlines, show.hlines = x$show.hlines, 
                                         show.namesep = x$show.namesep, show.csep = x$show.csep, 
                                         show.rsep = x$show.rsep)
     upViewport()
}