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
  
      #if(nrow(d1)==0)
      #{
      #  return(textGrob(""))
      #}
      
      
    
      #if(nrow(d)>0)
      #{
        #d <- d[,value_numeric:=labels[value_numeric]]
        
        d <- d1[,value_numeric:=factor(value_numeric,levels=1:7)]
        d <- d[,value_numeric:=factor(value_numeric,levels=rev(levels(value_numeric)))]
      
        p <- d[,sum(end_point-begin_point),by=list(value_numeric)]
        
        p <- merge(data.table(value_numeric=factor(7:1)),p,by="value_numeric",all.x=TRUE)
        p <- cbind(type=barcolor,p)
        
        p <- p[,         type:=factor(type)]

        p[is.na(V1),type:="white"]
        p[is.na(V1),V1:=0]
        
        #break.values <- c(round(scale/50,0)*25,round(scale/50,0)*50)
        
        p <- p[,value_numeric:=factor(value_numeric,levels=rev(levels(value_numeric)))]
        
        p[,V1:=V1/sum(V1)]
        p[is.na(V1),V1:=0]
        
        p <- ggplot(p,aes(x=value_numeric,y=V1,fill=type))
        p <- p + geom_bar(aes(width=0.8),stat="identity",position=position_dodge(0.0))
        #p <- p + scale_y_continuous("",breaks=break.values,limits=c(0,break.values[2]+100))
        p <- p + scale_y_continuous(labels=percent,breaks=c(0,0.5,1),limits=c(0,1))
        p <- p + coord_flip()
        p <- p + ggtitle(title)
        p <- p + scale_fill_manual("",values=c("slategray"="slategray","gray"="gray75","black"="black","white"="white"))
        #p <- p + facet_grid(. ~ type)

        p <- p + theme_minimal() + 
                 theme(
                      
                      #axis.text.y=element_text(size = 3.5, angle = 0, hjust = 1,colour="slategray"), 
                      strip.text.x = element_text(size = 8, angle = 0),
                      strip.text.y = element_text(size = 8, angle = 0),
                      axis.ticks=element_blank(),
                      axis.title.x=element_blank(),
                      axis.title.y=element_blank(),
                      plot.title = element_text(size=6.1,face="bold",colour="slategray",hjust=0),
                      legend.position="none",
                      #plot.title = element_text(size=6, face="bold",colour = "slategray"),
                      #panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank(),
                      panel.background = element_blank(),
                      axis.line = element_line(colour = "white"),
                      plot.margin = unit(c(topMargin,leftMargin,bottomMargin,rightMargin), "cm")
                 )
        
        if(showAxis)
        {
          p <- p + theme(axis.text.x=element_text(size=4.5, angle=90,hjust = 1,colour="slategray"))  
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
      #} else {
      #  return(textGrob(""))
      #}
      
      
  
}
  
  
  