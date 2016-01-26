###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2015
# Author: Jeff Dumont
#
#
# Description:
#
# Author needs to add a description!
#
###########################################################################

densityPlot <- function(
     d1,
     d2,
     d3=NULL,
     title="",
     minvalue,
     maxvalue,
     year1,
     year2,
     topMargin=0,leftMargin=0,bottomMargin=0,rightMargin=0,
     showLabel=FALSE,
     showXaxis=FALSE,
     ymax)
{
     
     adjustment <- 1#c(1,1)[densitytype]
     
     if(nrow(d1)>2|nrow(d2)>2) # we have something to report (density plots require at least 3 points to draw)
     {
          
          #p <- ggplot(data = d1, aes(x = value_numeric))
          
          #if(nrow(d1)>0)
          #{
          #     p <- p + geom_density(data = d1, color="slategray", linetype="solid", size=0.2,fill="white",adjust=adjustment,aes(weights=(end_point-begin_point)/sum(end_point-begin_point)))

          #}
          
          #if(nrow(d2)>0)
          #{
          #     p <- p + geom_density(data = d2, color ="gray75", linetype="dashed", size=0.2,fill="white",adjust=adjustment,aes(weights=(end_point-begin_point)/sum(end_point-begin_point)))
          #}
          
          #if(!is.null(d3))
          #{
          #     p <- p + geom_density(data = d3, color ="black", linetype="dotted", size=0.2,fill="white",adjust=adjustment,aes(weights=(end_point-begin_point)/sum(end_point-begin_point)))
          #}
          
          p1 <- ggplot(data = d1, aes(x = value_numeric))
          p2 <- ggplot(data = d1, aes(x = value_numeric))
          p3 <- ggplot(data = d1, aes(x = value_numeric))
          
          if(nrow(d1)>2)
          {
               p1 <- p1 + geom_density(data = d1, color="slategray", linetype="solid", size=0.25,fill="white",adjust=adjustment,aes(weights=(end_point-begin_point)/sum(end_point-begin_point)))
               #p1 <- p1 + stat_density(geom="line",data = d1, color="slategray", linetype="solid", size=0.35,fill="white",adjust=adjustment,aes(weights=(end_point-begin_point)/sum(end_point-begin_point)))
               if(showLabel)
               {
                  p1 <- p1 + ylab(year1)
               } else
               {
                  p1 <- p1 + ylab("") 
                 
               }
                 
          } else {
               p1 <- p1 + geom_density(data = d3, color ="white", linetype="solid", size=0.25,fill="white",adjust=adjustment,aes(weights=(end_point-begin_point)/sum(end_point-begin_point)))    
            
          }
          
          if(nrow(d2)>2)
          {
               p2 <- p2 + geom_density(data = d2, color ="gray75", linetype="solid", size=0.25,fill="white",adjust=adjustment,aes(weights=(end_point-begin_point)/sum(end_point-begin_point)))
               #p2 <- p2 + stat_density(geom="line",data = d2, color ="gray75", linetype="solid", size=0.35,fill="white",adjust=adjustment,aes(weights=(end_point-begin_point)/sum(end_point-begin_point)))
               if(showLabel)
               {
                  p2 <- p2 + ylab(year2)
               } else
               {
                  p2 <- p2 + ylab("") 
                 
               }
          } else {
              p2 <- p2 + geom_density(data = d3, color ="white", linetype="solid", size=0.25,fill="white",adjust=adjustment,aes(weights=(end_point-begin_point)/sum(end_point-begin_point)))  
          }
          
          if(!is.null(d3))
          {
               p3 <- p3 + geom_density(data = d3, color ="black", linetype="solid", size=0.25,fill="white",adjust=adjustment,aes(weights=(end_point-begin_point)/sum(end_point-begin_point)))
               #p3 <- p3 + stat_density(geom="line",data = d3, color ="black", linetype="solid", size=0.35,fill="white",adjust=adjustment,aes(weights=(end_point-begin_point)/sum(end_point-begin_point)))
               if(showLabel)
               {
                  p3 <- p3 + ylab("National")
               } else
               {
                  p3 <- p3 + ylab("") 
                 
               }
          }
          
          #p1 <- p1 + ggtitle(title)
          
          #p <- p + 
          
          p1 <- p1 +     #geom_density(data = var.national,color="black", linetype="twodash", size=0.25) +
               theme_minimal() + 
               scale_y_continuous(limits=c(0,ymax))+ 
               scale_x_continuous(labels = comma,limits=c(minvalue,maxvalue)) +
               #xlim(c(minvalue,maxvalue))+
               theme(
                    axis.text.x=element_blank(),#element_text(size=6, hjust = 0,colour="slategray"),
                    axis.text.y=element_blank(),#element_text(size=6, angle = 0, hjust = 0,colour="slategray"), 
                    strip.text.x = element_text(size = 8, angle = 0),
                    strip.text.y = element_text(size = 8, angle = 0),
                    axis.ticks=element_blank(),
                    axis.title.x=element_blank(),
                    axis.title.y=element_text(size=5, face="bold", angle = 90, hjust = 0.5,colour="slategray"),
                    plot.title = element_blank(),#element_text(size=6.1, face="bold",colour = "slategray"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "white"),
                    plot.margin = unit(c(topMargin,leftMargin,bottomMargin,rightMargin), "cm")
               )
          
          mp <- (maxvalue-minvalue)/2
          yp <- ymax/1.10
          
          p1 <- p1 + annotate("text", x =mp, y=yp, label = title,size=2.1, hjust=1, face="bold",colour = "slategray")
          
          p2 <- p2 +     #geom_density(data = var.national,color="black", linetype="twodash", size=0.25) +
               theme_minimal() + 
               scale_y_continuous(limits=c(0,ymax))+ 
               scale_x_continuous(labels = comma,limits=c(minvalue,maxvalue)) +
               theme(
                    axis.text.x=element_blank(),#element_text(size=6, hjust = 0,colour="slategray"),
                    axis.text.y=element_blank(),#element_text(size=6, angle = 0, hjust = 0,colour="slategray"), 
                    strip.text.x = element_text(size = 8, angle = 0),
                    strip.text.y = element_text(size = 8, angle = 0),
                    axis.ticks=element_blank(),
                    axis.title.x=element_blank(),
                    axis.title.y=element_text(size=5, face="bold", angle = 90, hjust = 0.5,colour="gray75"),
                    plot.title = element_text(size=6.1, face="bold",colour = "slategray"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "white"),
                    plot.margin = unit(c(topMargin,leftMargin,bottomMargin,rightMargin), "cm")
               )
          
          p3 <- p3 +     #geom_density(data = var.national,color="black", linetype="twodash", size=0.25) +
               theme_minimal() + 
               scale_y_continuous(limits=c(0,ymax))+ 
               scale_x_continuous(labels = comma,limits=c(minvalue,maxvalue))  +
               theme(
                    axis.text.x=element_text(size=4.5, angle=90, hjust = 1,colour="slategray"),
                    axis.text.y=element_blank(),#element_text(size=6, angle = 0, hjust = 0,colour="slategray"), 
                    strip.text.x = element_text(size = 8, angle = 0),
                    strip.text.y = element_text(size = 8, angle = 0),
                    axis.ticks=element_blank(),
                    axis.title.x=element_blank(),
                    axis.title.y=element_text(size=5, face="bold", angle = 90, hjust = 0.5,colour="black"),
                    plot.title = element_text(size=6.1, face="bold",colour = "slategray"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "white"),
                    plot.margin = unit(c(topMargin,leftMargin,bottomMargin,rightMargin), "cm")
               )
          
          p <- arrangeGrob(p1,p2,p3,nrow=3)
          
          return(p)
     } else
     {
          return(
            arrangeGrob(textGrob(paste0(title,"\ndata is not available"),just="top",gp=gpar(fontsize=7, fontface="bold",col = "red")),
                        heights=unit(1,units="npc"))
          )
     }
     
}