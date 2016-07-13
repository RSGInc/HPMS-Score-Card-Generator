###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont
#
#
# Description:
#
# Graphical bar plot for overall condition analysis in the pavement: detailed 
# review page.
#
###########################################################################

barOCPlot <- function(
      d1,
      title=""
)
{
      if(is.null(d1))
      {
        return(textGrob(""))
      }
  
      d1[is.na(overallscore),overallscore:="NA"]
    
      p <- merge(data.table(overallscore=c("NA","G","F","P")),d1,by="overallscore",all.x=TRUE)

      p[is.na(miles),miles:=0]
      p[is.na(miles),N:=0]
        
      p[,milesperc:=miles/sum(miles)]
      
      p[overallscore=="G" ,color:="slategray"]
      p[overallscore=="F" ,color:="gray85"]
      p[overallscore=="P" ,color:="gray65"]
      p[overallscore=="NA",color:="black"]

      p[overallscore=="NA",overallscore:="Insufficient\nInformation"]
      p[overallscore=="P",overallscore:="Poor"]
      p[overallscore=="F",overallscore:="Fair"]
      p[overallscore=="G",overallscore:="Good"]
            
      p[,overallscore:=factor(overallscore,levels=c("Insufficient\nInformation","Poor","Fair","Good"))]
        
      p <- ggplot(p[c(4,3,2,1),],aes(x=overallscore,y=milesperc,fill=color))
      p <- p + geom_bar(aes(width=0.6),stat="identity",position=position_dodge(0.0))+
               geom_text(aes(label = paste0(round(milesperc*100,1),"%"),y=milesperc+0.075), size = 2)
      p <- p + scale_y_continuous(labels=percent,breaks=c(0,0.5,1),limits=c(0,2.0))
      p <- p + coord_flip()
      p <- p + ylab(label="")
      p <- p + xlab(label=title)
      
      p <- p + scale_fill_manual("",values=c("slategray"="slategray","gray85"="gray85","gray65"="gray65","black"="black"))

      p <- p + theme_minimal() + 
               theme(
                      strip.text.x = element_text(size = 8, angle = 0),
                      strip.text.y = element_text(size = 8, angle = 0),
                      axis.ticks=element_blank(),
                      axis.title.x=element_text(size=8,face="bold",colour="slategray",hjust=0.5,angle=90),
                      axis.title.y=element_text(size=8,face="bold",colour="slategray",hjust=0.5),
                      legend.position="none",
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank(),
                      panel.background = element_blank(),
                      axis.line = element_line(colour = "white"),
                      plot.margin = unit(c(0,-3,0,-3), "cm")
                 )
        
        p <- p + theme(axis.text.x=element_blank())
        p <- p + theme(axis.text.y=element_text(size=5, hjust = 1,colour="slategray"))

        return(p)

}