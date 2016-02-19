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

create_title_page <- function(data,state,year,year_compare=NULL)
{
     
    #title_text <- read.table("resources\\dat\\title_text.txt",sep="@",as.is=TRUE,allowEscapes=TRUE,header=FALSE)
  
    grid.arrange(
          arrangeGrob( 
               rectGrob(gp = gpar(fill = "slategray2", col="slategray2")),
               rectGrob(gp = gpar(fill = "white", col="white")), 
               rectGrob(gp = gpar(fill = "slategray2", col="slategray2")), 
               
               arrangeGrob(
                         rectGrob(gp = gpar(fill = "white", col="white")),rectGrob(gp = gpar(fill = "white", col="white")),rectGrob(gp = gpar(fill = "white", col="white")),
                         rectGrob(gp = gpar(fill = "white", col="white")),rectGrob(gp = gpar(fill = "gray90", col="gray90")),rectGrob(gp = gpar(fill = "white", col="white")),
                         rectGrob(gp = gpar(fill = "white", col="white")),rectGrob(gp = gpar(fill = "white", col="white")),rectGrob(gp = gpar(fill = "white", col="white")),
                         rectGrob(gp = gpar(fill = "white", col="white")),rectGrob(gp = gpar(fill = "white", col="white")),rectGrob(gp = gpar(fill = "white", col="white")),
                         rectGrob(gp = gpar(fill = "white", col="white")),rectGrob(gp = gpar(fill = "white", col="white")),rectGrob(gp = gpar(fill = "white", col="white")),
               nrow=5,ncol=3,widths=unit(c(0.035,0.93,0.035),units="npc"),heights=unit(c(0.06,0.15,0.765,0.03,0.005),units="npc")
               ),
               nrow = 1,
               widths=unit(c(0.2,0.005,0.005,0.79),units="npc")
          ),
          nrow=1,
          heights = unit(1,units="npc"))
     
     grid.text("HPMS SCORECARD", 
               x = 0.03, 
               y = 0.68, 
               just = "left", 
               gp = gpar(col = "slategray",fontface = "bold", fontsize = 10)
     )
     
     grid.text(gState_Labels[index==state,label], 
               x = 0.03, 
               y = 0.63, 
               just = "left", 
               gp = gpar(col = "black", fontface = "bold", fontsize = 18)
     )
     
     grid.text(year, 
               x = 0.03, 
               y = 0.57, 
               just = "left", 
               gp = gpar(col = "black", fontface = "bold", fontsize = 28)
     )
     
     grid.text(paste0("Generated: ",format(Sys.time(), "%B %d, %Y")), 
               x = 0.03, 
               y = 0.52, 
               just = "left", 
               gp = gpar(col = "black", fontface = "italic", fontsize = 7)
     )
     
     grid.text(title_text, 
               x = 0.03, 
               y = 0.285, 
               just = "left", 
               gp = gpar(col = "black", fontsize = 6.5)
     )
     
     
     
     vertical_adj <- 0.05
     
     #grid.draw(ellipseGrob(rep(0.25,6),c(0.14,0.21,0.38,0.53,0.66,0.818)-vertical_adj,size=15,ar=1,angle=0,
     #                 def="npc", gp=gpar(fill="slategray",col="slategray")))
     
     #grid.draw(ellipseGrob(0.25,0.956,size=15,ar=1,angle=0,
     #                     def="npc", gp=gpar(fill="red",col="red")))
     
     #grid.text("s" ,0.25,0.961,gp = gpar(col = "white",fontface = "bold", fontsize = 18))
     #grid.text("i" ,0.25,0.820-vertical_adj,gp = gpar(col = "white", fontface = "bold", fontsize = 18))
     #grid.text("p" ,0.25,0.665-vertical_adj,gp = gpar(col = "white", fontface = "bold", fontsize = 18))
     #grid.text("t" ,0.25,0.530-vertical_adj,gp = gpar(col = "white", fontface = "bold", fontsize = 18))
     #grid.text("g" ,0.25,0.382-vertical_adj,gp = gpar(col = "white", fontface = "bold", fontsize = 18))
     #grid.text("r" ,0.25,0.21-vertical_adj,gp = gpar(col = "white", fontface = "bold", fontsize = 18))
     #grid.text("sn",0.25,0.14-vertical_adj,gp = gpar(col = "white", fontface = "bold", fontsize = 18))
     
     grid.text("summary"         ,0.25,0.925,hjust=0,gp = gpar(col = "steelblue4", fontface = "bold", fontsize = 13))
     grid.text("inventory"       ,0.25,0.823-vertical_adj,hjust=0,gp = gpar(col = "steelblue4", fontface = "bold", fontsize = 13))
     grid.text("pavement"        ,0.25,0.665-vertical_adj,hjust=0,gp = gpar(col = "steelblue4", fontface = "bold", fontsize = 13))
     grid.text("traffic"         ,0.25,0.535-vertical_adj,hjust=0,gp = gpar(col = "steelblue4", fontface = "bold", fontsize = 13))
     grid.text("geometric"       ,0.25,0.387-vertical_adj,hjust=0,gp = gpar(col = "steelblue4", fontface = "bold", fontsize = 13))
     grid.text("route"           ,0.25,0.237-vertical_adj,hjust=0,gp = gpar(col = "steelblue4", fontface = "bold", fontsize = 13))
     grid.text("special networks",0.25,0.14-vertical_adj,hjust=0,gp = gpar(col = "steelblue4", fontface = "bold", fontsize = 13))
     
     #grid.draw(rectGrob(x=0.6,y=0.8,width=0.6,heigh=0.3,gp=gpar(fill="gray80",col="gray80")))
     
     # summary
     #grid.draw(linesGrob(x = unit(c(0.25, 0.97), "npc"),
     #                     y = unit(c(0.9425,0.9425), "npc"),
     #                     gp=gpar(col="slategray",lty=3)))
     
     # inventory
     grid.draw(linesGrob(x = unit(c(0.25, 0.97), "npc"),
                         y = unit(c(0.8045,0.8045)-vertical_adj, "npc"),
                         gp=gpar(col="slategray",lty=3)))
     
     # pavement
     grid.draw(linesGrob(x = unit(c(0.25, 0.97), "npc"),
                         y = unit(c(0.6465,0.6465)-vertical_adj, "npc"),
                         gp=gpar(col="slategray",lty=3)))
     
     # traffic
     grid.draw(linesGrob(x = unit(c(0.25, 0.97), "npc"),
                         y = unit(c(0.5165,0.5165)-vertical_adj, "npc"),
                         gp=gpar(col="slategray",lty=3)))
     
     # geometric
     grid.draw(linesGrob(x = unit(c(0.25, 0.97), "npc"),
                         y = unit(c(0.3685,0.3685)-vertical_adj, "npc"),
                         gp=gpar(col="slategray",lty=3)))
     
     # route
     grid.draw(linesGrob(x = unit(c(0.25, 0.97), "npc"),
                         y = unit(c(0.2185,0.2185)-vertical_adj, "npc"),
                         gp=gpar(col="slategray",lty=3)))
     
     # special network
     grid.draw(linesGrob(x = unit(c(0.25, 0.97), "npc"),
                         y = unit(c(0.12,0.12)-vertical_adj, "npc"),
                         gp=gpar(col="slategray",lty=3)))
     
     cat("\nCalculating coverage validation results. This may take some time to complete.")
     
     # Inventory
     R <- 1
     C <- 1
     startx <- 0.35
     starty <- 0.79-vertical_adj
     
     rowWidth <- 0.020
     
     for(i in 1:length(gVariables[,Name]))
     {
          
          if(gVariables[i,Grouping]=="I")
          {
               grid.draw(textGrob(gVariables[i,Name],x=startx+(C-1)*0.15,starty-(R-1)*rowWidth,hjust=1,gp=gpar(col="slategray",fontsize=7)))
               #grid.ellipse(x=startx+(C-1)*0.15+0.01,y=starty-(R-1)*0.0175,size=2.5,ar=1,angle=0,def="npc",gp=gpar(fill="white",col="slategray"))
               
               plotRect(data,year,gVariables[i,Name],startx,starty,C,R)
            
               if(R < 4)
               {
                    R <- R + 1
               } else
               {
                    R <- 1
                    C <- 1 + C
               }
          }
     }
     
     # Pavement
     R <- 1
     C <- 1
     startx <- 0.35
     starty <- 0.63-vertical_adj
     for(i in 1:length(gVariables[,Name]))
     {
          
          if(gVariables[i,Grouping]=="P")
          {
               grid.draw(textGrob(gVariables[i,Name],x=startx+(C-1)*0.15,starty-(R-1)*rowWidth,hjust=1,gp=gpar(col="slategray",fontsize=7)))
               #grid.ellipse(x=startx+(C-1)*0.15+0.01,y=starty-(R-1)*0.0175,size=2.5,ar=1,angle=0,def="npc",gp=gpar(fill="white",col="red"))
               plotRect(data,year,gVariables[i,Name],startx,starty,C,R)
               if(R < 3)
               {
                    R <- R + 1
               } else
               {
                    R <- 1
                    C <- 1 + C
               }
          }
     }
     
     # Traffic
     R <- 1
     C <- 1
     startx <- 0.35
     starty <- 0.5-vertical_adj
     for(i in 1:length(gVariables[,Name]))
     {
          
          if(gVariables[i,Grouping]=="T")
          {
               grid.draw(textGrob(gVariables[i,Name],x=startx+(C-1)*0.15,starty-(R-1)*rowWidth,hjust=1,gp=gpar(col="slategray",fontsize=7)))
               #grid.ellipse(x=startx+(C-1)*0.15+0.01,y=starty-(R-1)*0.0175,size=2.5,ar=1,angle=0,def="npc",gp=gpar(fill="red",col="red"))
               plotRect(data,year,gVariables[i,Name],startx,starty,C,R)
               if(R < 4)
               {
                    R <- R + 1
               } else
               {
                    R <- 1
                    C <- 1 + C
               }
          }
     }
     
     # Geometric
     R <- 1
     C <- 1
     startx <- 0.35
     starty <- 0.35-vertical_adj
     for(i in 1:length(gVariables[,Name]))
     {
          
          if(gVariables[i,Grouping]=="G")
          {
               grid.draw(textGrob(gVariables[i,Name],x=startx+(C-1)*0.15,starty-(R-1)*rowWidth,hjust=1,gp=gpar(col="slategray",fontsize=7)))
               #grid.ellipse(x=startx+(C-1)*0.15+0.01,y=starty-(R-1)*0.0175,size=2.5,ar=1,angle=0,def="npc",gp=gpar(fill="slategray",col="slategray"))
               plotRect(data,year,gVariables[i,Name],startx,starty,C,R)
               if(R < 5)
               {
                    R <- R + 1
               } else
               {
                    R <- 1
                    C <- 1 + C
               }
          }
     }
     
     # ROute
     R <- 1
     C <- 1
     startx <- 0.35
     starty <- 0.2-vertical_adj
     for(i in 1:length(gVariables[,Name]))
     {
          
          if(gVariables[i,Grouping]=="R")
          {
               grid.draw(textGrob(gVariables[i,Name],x=startx+(C-1)*0.15,starty-(R-1)*rowWidth,hjust=1,gp=gpar(col="slategray",fontsize=7)))
               #grid.ellipse(x=startx+(C-1)*0.15+0.01,y=starty-(R-1)*0.0175,size=2.5,ar=1,angle=0,def="npc",gp=gpar(fill="red",col="red"))
               plotRect(data,year,gVariables[i,Name],startx,starty,C,R)
               if(R < 1)
               {
                    R <- R + 1
               } else
               {
                    R <- 1
                    C <- 1 + C
               }
          }
     }
     
     # speical networks
     R <- 1
     C <- 1
     startx <- 0.35
     starty <- 0.105-vertical_adj
     for(i in 1:length(gVariables[,Name]))
     {
          
          if(gVariables[i,Grouping]=="SN")
          {
               grid.draw(textGrob(gVariables[i,Name],x=startx+(C-1)*0.15,starty-(R-1)*rowWidth,hjust=1,gp=gpar(col="slategray",fontsize=7)))
               #grid.ellipse(x=startx+(C-1)*0.15+0.01,y=starty-(R-1)*0.0175,size=2.5,ar=1,angle=0,def="npc",gp=gpar(fill="slategray",col="slategray"))
               plotRect(data,year,gVariables[i,Name],startx,starty,C,R)
               if(R < 1)
               {
                    R <- R + 1
               } else
               {
                    R <- 1
                    C <- 1 + C
               }
          }
     }
     
     
     # legend
     
     grid.draw(linesGrob(x = unit(c(0.25, 0.97), "npc"),
                         y = unit(c(0.03,0.03), "npc"),
                         gp=gpar(col="gray60")))
     
     xshift <- 0.05
     
     #grid.ellipse(x=0.35+xshift,y=0.015,size=2.5,ar=1,angle=0,def="npc",gp=gpar(fill="slategray",col="slategray"))
     #grid.ellipse(x=0.50+xshift ,y=0.015,size=2.5,ar=1,angle=0,def="npc",gp=gpar(fill="red",col="red"))
     #grid.ellipse(x=0.65+xshift,y=0.015,size=2.5,ar=1,angle=0,def="npc",gp=gpar(fill="white",col="slategray"))
     #grid.ellipse(x=0.80+xshift ,y=0.015,size=2.5,ar=1,angle=0,def="npc",gp=gpar(fill="white",col="red"))
     
     grid.rect(x=0.34+xshift,
                         y=0.015,
                         width=unit(0.007,"npc"),
                         height=unit(0.0125,"npc"),
                         gp=gpar(fill="slategray",col="slategray"))
     
     grid.rect(x=0.49+xshift,
                         y=0.015,
                         width=unit(0.007,"npc"),
                         height=unit(0.0125,"npc"),
                         gp=gpar(fill="gray75",col="slategray"))
     
     grid.rect(x=0.64+xshift,
                         y=0.015,
                         width=unit(0.007,"npc"),
                         height=unit(0.0125,"npc"),
                         gp=gpar(fill="white",col="slategray"))
     
     grid.text("Submitted and Complete",x=0.355+xshift, y=0.015,hjust=0,gp=gpar(col="slategray",fontface="italic",fontsize=6))
     grid.text("Submitted and Incomplete",x=0.505+xshift, y=0.015,hjust=0,gp=gpar(col="slategray",fontface="italic",fontsize=6))
     grid.text("Not Submitted",x=0.655+xshift, y=0.015,hjust=0,gp=gpar(col="slategray",fontface="italic",fontsize=6))
     #grid.text("Not Submitted but Worth Exploring",x=0.805+xshift, y=0.015,hjust=0,gp=gpar(col="slategray",fontface="italic",fontsize=6))
     
     grid.text("Key to data item status: ",x=0.31+xshift, y=0.015,hjust=1,gp=gpar(col="steelblue4",fontface="bold",fontsize=8))
     
     # logos
     grid.raster(image=gLogo,x = 0.03, y=0.85,just = "left", width = 0.13)
     grid.raster(image=gLogo2,x = 0.02, y=0.04,just = "left", width = 0.10)

     # summary section of the report
     
     #grid.text("Overall Score",x=0.43,y=0.925,just="centre",gp=gpar(fontsize=12, fontface="bold", col="slategray"))
     #grid.text("85",x=0.43,y=0.865,just="centre",gp=gpar(fontsize=50, fontface="bold", col="black"))

     grid.text("Timeliness",  x=0.41,y=0.915,just="right",gp=gpar(fontsize=10, col="slategray"))
     grid.text("Completeness",x=0.41,y=0.87 ,just="right",gp=gpar(fontsize=10, col="slategray"))
     grid.text("Quality",     x=0.41,y=0.825,just="right",gp=gpar(fontsize=10, col="slategray"))

     grid.text("??",x=0.42,y=0.915,just="left",gp=gpar(fontsize=14, fontface="bold", col="steelblue4"))
     grid.text("??",x=0.42,y=0.87 ,just="left",gp=gpar(fontsize=14, fontface="bold", col="steelblue4"))
     grid.text("??",x=0.42,y=0.825,just="left",gp=gpar(fontsize=14, fontface="bold", col="steelblue4"))

          
     results <- create_overall_report(data,state,year)

     #results <- data.frame(results)
               
     grid.text(paste0("Year ",year  ),x=0.660,y=0.925,gp=gpar(fontsize=9, fontface="bold", col="slategray"),hjust=1)
     grid.text(paste0("Year ",year-1),x=0.720,y=0.925,gp=gpar(fontsize=9, fontface="bold", col="slategray"),hjust=1)
     grid.text(paste0("Year ",year-2),x=0.780,y=0.925,gp=gpar(fontsize=9, fontface="bold", col="slategray"),hjust=1)
     grid.text(paste0("Year ",year-3),x=0.840,y=0.925,gp=gpar(fontsize=9, fontface="bold", col="slategray"),hjust=1)
     grid.text(paste0("Year ",year-4),x=0.900,y=0.925,gp=gpar(fontsize=9, fontface="bold", col="slategray"),hjust=1)
     
     grid.text(results[1,1,with=FALSE],x=0.60,y=0.900,gp=gpar(fontsize=8, fontface="bold", col="slategray"),hjust=1)
     grid.text(results[2,1,with=FALSE],x=0.60,y=0.875,gp=gpar(fontsize=8, fontface="bold", col="slategray"),hjust=1)
     grid.text(results[3,1,with=FALSE],x=0.60,y=0.850,gp=gpar(fontsize=8, fontface="bold", col="slategray"),hjust=1)
     grid.text(results[4,1,with=FALSE],x=0.60,y=0.825,gp=gpar(fontsize=8, fontface="bold", col="slategray"),hjust=1)

     grid.text(results[1,2,with=FALSE],x=0.660,y=0.900,gp=gpar(fontsize=8, col="black"),hjust=1)
     grid.text(results[2,2,with=FALSE],x=0.660,y=0.875,gp=gpar(fontsize=8, col="black"),hjust=1)
     grid.text(results[3,2,with=FALSE],x=0.660,y=0.850,gp=gpar(fontsize=8, col="black"),hjust=1)
     grid.text(results[4,2,with=FALSE],x=0.660,y=0.825,gp=gpar(fontsize=8, col="black"),hjust=1)
     
     grid.text(results[1,3,with=FALSE],x=0.720,y=0.900,gp=gpar(fontsize=8, col="black"),hjust=1)
     grid.text(results[2,3,with=FALSE],x=0.720,y=0.875,gp=gpar(fontsize=8, col="black"),hjust=1)
     grid.text(results[3,3,with=FALSE],x=0.720,y=0.850,gp=gpar(fontsize=8, col="black"),hjust=1)
     grid.text(results[4,3,with=FALSE],x=0.720,y=0.825,gp=gpar(fontsize=8, col="black"),hjust=1)
     
     grid.text(results[1,4,with=FALSE],x=0.780,y=0.900,gp=gpar(fontsize=8, col="black"),hjust=1)
     grid.text(results[2,4,with=FALSE],x=0.780,y=0.875,gp=gpar(fontsize=8, col="black"),hjust=1)
     grid.text(results[3,4,with=FALSE],x=0.780,y=0.850,gp=gpar(fontsize=8, col="black"),hjust=1)
     grid.text(results[4,4,with=FALSE],x=0.780,y=0.825,gp=gpar(fontsize=8, col="black"),hjust=1)
     
     grid.text(results[1,5,with=FALSE],x=0.840,y=0.900,gp=gpar(fontsize=8, col="black"),hjust=1)
     grid.text(results[2,5,with=FALSE],x=0.840,y=0.875,gp=gpar(fontsize=8, col="black"),hjust=1)
     grid.text(results[3,5,with=FALSE],x=0.840,y=0.850,gp=gpar(fontsize=8, col="black"),hjust=1)
     grid.text(results[4,5,with=FALSE],x=0.840,y=0.825,gp=gpar(fontsize=8, col="black"),hjust=1)
     
     grid.text(results[1,6,with=FALSE],x=0.900,y=0.900,gp=gpar(fontsize=8, col="black"),hjust=1)
     grid.text(results[2,6,with=FALSE],x=0.900,y=0.875,gp=gpar(fontsize=8, col="black"),hjust=1)
     grid.text(results[3,6,with=FALSE],x=0.900,y=0.850,gp=gpar(fontsize=8, col="black"),hjust=1)
     grid.text(results[4,6,with=FALSE],x=0.900,y=0.825,gp=gpar(fontsize=8, col="black"),hjust=1)
     
     #grid.text(
     #               create_overall_report(state,year,year_compare),
     #               
                    #rows=NULL, 
                    #core.just = "right",
                    #col.just="right",
                    #gpar.coretext = gpar(col = "black",fontsize=6.5),
                    #gpar.coltext = gpar(col = "black",fontsize=7, fontface = "bold"),
                    #padding.h=unit(0.1,units="inches"),padding.v=unit(0.1,units="inches")
     #)
     
     #add_page_number(1)
}