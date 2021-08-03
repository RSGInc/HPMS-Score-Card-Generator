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
  
  d_plt <- merge(data.table(overallscore=c("NA","G","F","P")),d1,by="overallscore",all.x=TRUE)
  
  d_plt[is.na(miles),miles:=0]
  d_plt[is.na(miles),N:=0]
  
  d_plt[,milesperc:=miles/sum(miles)]
  
  d_plt[overallscore=="G" ,color:="slategray"]
  d_plt[overallscore=="F" ,color:="gray85"]
  d_plt[overallscore=="P" ,color:="gray65"]
  d_plt[overallscore=="NA",color:="black"]
  
  d_plt[overallscore=="NA",overallscore:="Insufficient\nInformation"]
  d_plt[overallscore=="P",overallscore:="Poor"]
  d_plt[overallscore=="F",overallscore:="Fair"]
  d_plt[overallscore == "G", overallscore := "Good"]
  
  d_plt[, overallscore := factor(overallscore,
                                 levels=c("Insufficient\nInformation", "Poor", "Fair", "Good"))]
  
  p <- ggplot(d_plt[c(4,3,2,1),], aes(x=overallscore, y=milesperc, fill=color)) +
    geom_bar(width=0.6,stat="identity",position=position_dodge(0.0))+
    geom_text(aes(label = paste0(round(milesperc*100,1),"%"),y=milesperc+0.075), size = 2) +
    scale_y_continuous(labels=percent,breaks=c(0,0.5,1),limits=c(0,2.0)) +
    coord_flip() +
    ylab(label="") +
    xlab(label=title) +
    scale_fill_manual("", values=c("slategray"=gColors$dark,
                                   "gray85"="gray85",
                                   "gray65"="gray65",
                                   "black"=gColors$text)) +
    theme_minimal() + 
    theme(
      strip.text.x = element_text(size = 8, angle = 0),
      strip.text.y = element_text(size = 8, angle = 0),
      axis.ticks=element_blank(),
      axis.title.x=element_text(size=8,face="bold",colour=gColors$dark,hjust=0.5,angle=90),
      axis.title.y=element_text(size=8,face="bold",colour=gColors$dark,hjust=0.5),
      legend.position="none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = gColors$blank),
      plot.margin = unit(c(0,-3,0,-3), "cm")
    ) +
    
    theme(axis.text.x=element_blank()) +
    theme(axis.text.y=element_text(size=5, hjust = 1,colour=gColors$dark))
  
  return(p)
  
}