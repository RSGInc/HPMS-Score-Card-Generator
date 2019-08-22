###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont
#
#
# Description:
#
# The basic density plot used on the summary pages
#
###########################################################################

# Get max and min x values, max y across all three datasets

getLimits <- function(dt){

    
  # Get max and min x values, max y
  
  if(nrow(dt[!is.na(value_numeric), ]) > 2){
    minvalue <- quantile(dt[, value_numeric], probs=0.05, na.rm=TRUE)
    maxvalue <- quantile(dt[, value_numeric], probs=0.95, na.rm=TRUE)
    if(minvalue == maxvalue){
      maxvalue <- 1 + minvalue
    }
    
    # Get max y by fitting a weighted density
    x <- dt[, value_numeric]
    w <- dt[, (end_point - begin_point) / sum(end_point - begin_point)]
    w <- w[!is.na(x)]
    ymax <- max(density(x, weights=w, na.rm=TRUE)$y)
    
  } else {
    minvalue <- NULL
    maxvalue <- NULL
    ymax     <- NULL
  }
  
  list(minvalue=minvalue, maxvalue=maxvalue, ymax=ymax)
} # getLimits


densityPlot <- function(
  d1,         # Current year data.table
  d2,         # Comparison year data.table
  d3=NULL,    # National data.table
  title="",
  year1,
  year2,
  topMargin=0,leftMargin=0,bottomMargin=0,rightMargin=0,
  showLabel=FALSE,
  showXaxis=FALSE){
     
  # Get x axis limits and y-axis maximum
  lims1 <- getLimits(d1)
  lims2 <- getLimits(d2)
  lims3 <- getLimits(d3)
  
  minvalue <- min(lims1$minvalue, lims2$minvalue) #, lims3$minvalue)
  maxvalue <- max(lims1$maxvalue, lims2$maxvalue) #, lims3$maxvalue)
  ymax     <- max(lims1$ymax, lims2$ymax, lims3$ymax3)*1.20

  ymax <- max(2.5*ymax,ymax + 0.05)
  adjustment <- 1#c(1,1)[densitytype]
  
  # we have something to report (density plots require at least 3 points to draw)
  if((nrow(d1)>2|nrow(d2)>2)&!is.null(minvalue)){
    
    p1 <- ggplot(data = d1, aes(x = value_numeric,weight=num_sections))
    p2 <- ggplot(data = d1, aes(x = value_numeric,weight=num_sections))
    p3 <- ggplot(data = d1, aes(x = value_numeric,weight=num_sections))
    
    if(nrow(d1)>2){
      p1 <- p1 +
        geom_density(data = d1, color="slategray", linetype="solid",
                     size=0.25,fill="slategray",
                     adjust=adjustment,
                     aes(weight=(end_point-begin_point)/sum(end_point-begin_point)))
      
    } else {
      p1 <- p1 +
        geom_density(data = d3, color ="white", linetype="solid",
                     size=0.25,fill="white",adjust=adjustment,
                     aes(weight=(end_point-begin_point)/sum(end_point-begin_point)))    
      
    }
    
    if(showLabel){
      p1 <- p1 + ylab(year1)
    } else {
      p1 <- p1 + ylab("") 
    }
    
    if(nrow(d2)>2) {
      p2 <- p2 + geom_density(data = d2, color ="gray75", linetype="solid", size=0.25,fill="gray75",adjust=adjustment,aes(weight=(end_point-begin_point)/sum(end_point-begin_point)))
      
    } else {
      p2 <- p2 + geom_density(data = d3, color ="white", linetype="solid", size=0.25,fill="white",adjust=adjustment,aes(weight=(end_point-begin_point)/sum(end_point-begin_point)))  
    }
    
    if(showLabel){
      p2 <- p2 + ylab(year2)
    } else {
      p2 <- p2 + ylab("") 
    }
    
    if(!is.null(d3)) {
      p3 <- p3 + geom_density(data = d3, color ="black", linetype="solid", size=0.25,fill="black",adjust=adjustment,aes(weight=(end_point-begin_point)/sum(end_point-begin_point)))
    }
    
    if(showLabel){
      p3 <- p3 + ylab("National")
    } else {
      p3 <- p3 + ylab("") 
    }
    
    
    p1 <- p1 +     
      theme_minimal() + 
      scale_y_continuous()+
      scale_x_continuous(labels = comma, limits=c(minvalue, maxvalue)) +
      
      theme(
        axis.text.x=element_text(size=4.5, angle=30, hjust = 1,colour="white"),
        axis.text.y=element_blank(),
        strip.text.x = element_text(size = 8, angle = 0),
        strip.text.y = element_text(size = 8, angle = 0),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=5, face="bold", angle = 90, hjust = 0.5,colour="slategray"),
        plot.title = element_text(size=6.1, face="bold",colour = "slategray", hjust=0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "white"),
        plot.margin = unit(c(topMargin=0,leftMargin=0,bottomMargin=0,rightMargin=0), "cm")
      )
    
    mp <- (maxvalue+minvalue)/2
    yp <- ymax*0.98
    
    if(is.na(mp)){
      mp <- 0.5
    }
    
    p1 <- p1 + ggtitle(title)
    p2 <- p2 + ggtitle(title)
    p3 <- p3 + ggtitle(title)
    
    p2 <- p2 +     
      theme_minimal() + 
      scale_y_continuous()+
      scale_x_continuous(labels = comma,limits=c(minvalue,maxvalue)) +
      theme(
        axis.text.x=element_text(size=4.5, angle=30, hjust = 1,colour="white"),
        axis.text.y=element_blank(),
        strip.text.x = element_text(size = 8, angle = 0),
        strip.text.y = element_text(size = 8, angle = 0),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=5, face="bold", angle = 90, hjust = 0.5,colour="gray75"),
        plot.title = element_text(size=6.1, face="bold",colour = "white", hjust=0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "white"),
        plot.margin = unit(c(topMargin=0,leftMargin=0,bottomMargin=0,rightMargin=0), "cm")
      )
    
    p3 <- p3 +     
      theme_minimal() + 
      scale_y_continuous()+
      scale_x_continuous(labels = comma,limits=c(minvalue,maxvalue))  +
      theme(
        axis.text.x=element_text(size=4.5, angle=30, hjust = 1,colour="slategray"),
        axis.text.y=element_blank(),
        strip.text.x = element_text(size = 8, angle = 0),
        strip.text.y = element_text(size = 8, angle = 0),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=5, face="bold", angle = 90, hjust = 0.5,colour="black"),
        plot.title = element_text(size=6.1, face="bold",colour = "white", hjust=0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "white"),
        plot.margin = unit(
          c(topMargin + 0, rightMargin + 0, bottomMargin + 0, leftMargin + 0), "cm")
      )
    
    p <- arrangeGrob(
      p1, p2, p3, 
      nrow=3, heights=unit(rep(1/3,3), units="npc")
    )
    
    return(p)
  } else {
    return(
      arrangeGrob(textGrob(paste0(title,"\ndata are not available or appropriate"),just="top",gp=gpar(fontsize=5,col = "red")),
                  heights=unit(1,units="npc"))
    )
  }
  
} # function