###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont
#
#
# Description:
#
# Creates donut chart for the overall condition report. This plot was deprecated
# in a later version of the generator. Not currently used.
#
###########################################################################

create_donut_chart <- function(results)
{
     results <- data.frame(results)
     names(results) <- c("category","count")
     
     results$category <- factor(results$category,levels=c("G","F","P"))
     
     results <- results[order(results$category),]
     
     results$fraction <- results$count / sum(results$count)
     
     results$ymax <- cumsum(results$fraction)
     results$ymin <- c(0, head(results$ymax, n=-1))
     
     y.breaks <- cumsum(results$fraction) - (results$fraction / 2)
     
     results$label <- paste0(round(results$fraction,2)*100,"%")
     
     p <- ggplot(results, aes(fill=category, ymax=ymax, ymin=ymin, xmax=5, xmin=3.5)) +
          geom_rect(colour=gColors$blank,fill=c("gray65","gray85",gColors$highlight)) +
          #geom_text(aes(x = count)) +
          coord_polar(theta="y") +
          xlim(c(0, 5)) +
          scale_y_continuous(breaks = y.breaks , labels = results$label) +
          theme_bw() +
          theme(panel.grid=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks=element_blank(),
                panel.border = element_blank(),
                text = element_text(size=8,colour=gColors$text, face="bold")
          ) +
          guides(fill = guide_legend(override.aes = list(colour = NA))) 
     
     return(p)
}