barPlot <- function(
      d1,
      d2,
      d3=NULL,
      labels,
      title=""
)
{
      if(nrow(d1)==0)
      {
        return(textGrob(""))
      }
      
      d <- cbind(type="red",d1)
    
      if(nrow(d2)>0)
      {
        d <- rbind(d,
                 cbind(type="blue",d2))
      }
      
      if(!is.null(d3))
      {
        d <- rbind(d,
                   cbind(type="black",d3)
                   )
      }
      
      if(nrow(d)>0)
      {
        d <- d[,value_numeric:=labels[value_numeric]]
        
        d <- d[,value_numeric:=factor(value_numeric)]
        d <- d[,         type:=factor(type)]
        
        p <- d[,sum(end_point-begin_point),by=list(type,value_numeric)]
        
        p <- ggplot(p,aes(x=value_numeric,y=V1,fill=type))
        p <- p + geom_bar(width=0.25,stat="identity",position=position_dodge(0.8))
        p <- p + coord_flip()
        p <- p + ggtitle(title)
        p <- p + scale_fill_manual("",values=c("red"="red","blue"="blue","black"="black"))
        #p <- p + facet_grid(. ~ type)
        p <- p + theme_minimal() + 
                 theme(
                      axis.text.x=element_text(size=6, hjust = 0,colour="slategray"),
                      axis.text.y=element_text(size = 0, angle = 0, hjust = 0), 
                      strip.text.x = element_text(size = 8, angle = 0),
                      strip.text.y = element_text(size = 8, angle = 0),
                      axis.ticks=element_blank(),
                      axis.title.x=element_blank(),
                      axis.title.y=element_blank(),
                      plot.title = element_text(size=7, face="bold",colour = "slategray"),
                      legend.position="none",
                      plot.title = element_text(size=7, face="bold",colour = "slategray"),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank(),
                      panel.background = element_blank(),
                      axis.line = element_line(colour = "white")
                 )
        return(p)
      } else {
        return(textGrob(""))
      }
      
      
  
}
  
  
  