###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont
#
#
# Description:
#
# General bar plot used in the summary pages
#
###########################################################################


barPlot <- function(
      d1,
      labels,
      title="",
      barcolor,
      topMargin=0,
      leftMargin=0,
      bottomMargin=0,
      rightMargin=0,
      showLabel=FALSE,
      showAxis=FALSE,
      scale
)
{
      if(is.null(d1))
      {
        return(textGrob(""))
      }
        
      d <- d1[,value_numeric:=factor(value_numeric,levels=1:7)]
      d <- d[,value_numeric:=factor(value_numeric,levels=rev(levels(value_numeric)))]
      
      p <- d[,sum(end_point-begin_point),by=list(value_numeric)]
        
      p <- merge(data.table(value_numeric=factor(7:1)),p,by="value_numeric",all.x=TRUE)
      p <- cbind(type=barcolor,p)
        
      p <- p[,         type:=factor(type)]

      p[is.na(V1),type:="white"]
      p[is.na(V1),V1:=0]
        
      p <- p[,value_numeric:=factor(value_numeric,levels=rev(levels(value_numeric)))]
        
      p[is.na(V1),V1:=0]
        
      p <- ggplot(p,aes(x=value_numeric,y=V1,fill=type))
      p <- p + geom_bar(width=0.8, stat="identity",position=position_dodge(0.0))
      p <- p + scale_y_continuous("",limits=c(0,scale))
      p <- p + coord_flip()
      p <- p + ggtitle(title)
      p <- p + scale_fill_manual("",values=c("slategray"="slategray","gray"="gray75","black"="black","white"="white"))

      p <- p + theme_minimal() + 
                 theme(
                      strip.text.x = element_text(size = 8, angle = 0),
                      strip.text.y = element_text(size = 8, angle = 0),
                      axis.ticks=element_blank(),
                      axis.title.x=element_blank(),
                      axis.title.y=element_blank(),
                      plot.title = element_text(size=6.1,face="bold",colour="slategray",hjust=0),
                      legend.position="none",
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank(),
                      panel.background = element_blank(),
                      axis.line = element_line(colour = "white"),
                      plot.margin = unit(
                        c(topMargin + 0.15, rightMargin + 0.15, bottomMargin + 0.15, leftMargin+0.15), "cm")
                 )
        
      if(showAxis)
      {
        p <- p + theme(axis.text.x=element_text(size=4.5, angle=90,hjust = 0.5,colour="slategray"))  
      } else
      {
        p <- p + theme(axis.text.x=element_blank())
      }
        
      if(showLabel)
      {
        p <- p + theme(axis.text.y=element_text(size=5, hjust = 1,colour="slategray"))
      } else
      {
        p <- p + theme(axis.text.y=element_blank())
      }
      
      return(p)

}
  
  
  