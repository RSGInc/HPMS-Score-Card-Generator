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
  
    scorestotals <- read.table("resources\\dat\\scoringweights.csv",sep=",",header=TRUE)
  
    timetotal     <- scorestotals[,"timeliness"]
    completetotal <- scorestotals[,"completeness"]
    qualitytotal  <- scorestotals[,"quality"]
    
    grid.arrange(
          arrangeGrob( 
               rectGrob(gp = gpar(fill = "gray90", col="gray90")),
               rectGrob(gp = gpar(fill = "white", col="white")), 
               rectGrob(gp = gpar(fill = "gray20", col="gray20")), 
               
               arrangeGrob(
                         rectGrob(gp = gpar(fill = "gray90", col="gray90")),rectGrob(gp = gpar(fill = "gray90", col="gray90")),rectGrob(gp = gpar(fill = "gray90", col="gray90")),
                         rectGrob(gp = gpar(fill = "gray90", col="gray90")),rectGrob(gp = gpar(fill = "gray90", col="gray90")),rectGrob(gp = gpar(fill = "gray90", col="gray90")),
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
     
     grid.draw(linesGrob(x = unit(c(0.03, 0.18), "npc"),
                         y = unit(c(0.66,0.66), "npc"),
                         gp=gpar(col="black",lty=1)))
     
     grid.text(toupper(gState_Labels[index==state,label]), 
               x = 0.03, 
               y = 0.63, 
               just = "left", 
               gp = gpar(col = "black", fontface = "bold", fontsize = 13)
     )
     
     grid.text(year, 
               x = 0.03, 
               y = 0.57, 
               just = "left", 
               gp = gpar(col = "black", fontface = "bold", fontsize = 27)
     )
     
     grid.draw(linesGrob(x = unit(c(0.03, 0.18), "npc"),
                         y = unit(c(0.505,0.505), "npc"),
                         gp=gpar(col="black",lty=1)))
     
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
               gp = gpar(col = "black", fontsize = 6)
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
     
     grid.text("Score"         ,0.25,0.965,hjust=0,gp = gpar(col = "steelblue4", fontface = "bold", fontsize = 13))
     grid.text("Data Summary"  ,0.835,0.965,hjust=1,gp = gpar(col = "steelblue4", fontface = "bold", fontsize = 13))
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
     
     CompletedScore <- 0
     TotalCompletedScore <- 0
     
     QualityScore <- 0
     submittedN   <- 0
     TotalQualityScore <- 0
     
     # Scores for getting a medium or high quality value
     QualityMed  <- CompleteMed  <- 1
     QualityHigh <- CompleteHigh <- 1.5
     
     for(i in 1:length(gVariables[,Name]))
     {
          
          if(gVariables[i,Grouping]=="I")
          {
               grid.draw(textGrob(gVariables[i,Name],x=startx+(C-1)*0.15,starty-(R-1)*rowWidth,hjust=1,gp=gpar(col="slategray",fontsize=7)))
               #grid.ellipse(x=startx+(C-1)*0.15+0.01,y=starty-(R-1)*0.0175,size=2.5,ar=1,angle=0,def="npc",gp=gpar(fill="white",col="slategray"))
               
               variable <- gVariables[i,Name]
            
               CompleteType <- plotRect(data,year,variable,startx,starty,C,R)
               
               CompletedScore      <- CompletedScore      + c(0,CompleteMed,CompleteHigh)[CompleteType] * gVariables[Name==variable,Completeness_Weight]
               TotalCompletedScore <- TotalCompletedScore +                 CompleteHigh * gVariables[Name==variable,Completeness_Weight]
               
               submittedN <- submittedN + 1 * ( CompleteType >= 2 )
               
               QualityType <- plotCircle(data,year,year_compare,variable,startx,starty,C,R)
               
               QualityScore      <- QualityScore      + c(0,QualityMed,QualityHigh)[QualityType] * gVariables[Name==variable,Quality_Weight]
               TotalQualityScore <- TotalQualityScore +                QualityHigh * gVariables[Name==variable,Quality_Weight] * ( CompleteType >= 2 )
               
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
               variable <- gVariables[i,Name]
            
               CompleteType <- plotRect(data,year,variable,startx,starty,C,R)
               
               CompletedScore      <- CompletedScore      + c(0,CompleteMed,CompleteHigh)[CompleteType] * gVariables[Name==variable,Completeness_Weight]
               TotalCompletedScore <- TotalCompletedScore +                 CompleteHigh * gVariables[Name==variable,Completeness_Weight]
               
               submittedN <- submittedN + 1 * ( CompleteType >= 2 )
               
               QualityType <- plotCircle(data,year,year_compare,variable,startx,starty,C,R)
               
               QualityScore      <- QualityScore      + c(0,QualityMed,QualityHigh)[QualityType] * gVariables[Name==variable,Quality_Weight]
               TotalQualityScore <- TotalQualityScore +                QualityHigh * gVariables[Name==variable,Quality_Weight] * ( CompleteType >= 2 )
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
               variable <- gVariables[i,Name]
            
               CompleteType <- plotRect(data,year,variable,startx,starty,C,R)
               
               CompletedScore      <- CompletedScore      + c(0,CompleteMed,CompleteHigh)[CompleteType] * gVariables[Name==variable,Completeness_Weight]
               TotalCompletedScore <- TotalCompletedScore +                 CompleteHigh * gVariables[Name==variable,Completeness_Weight]
               
               submittedN <- submittedN + 1 * ( CompleteType >= 2 )
               
               QualityType <- plotCircle(data,year,year_compare,variable,startx,starty,C,R)
               
               QualityScore      <- QualityScore      + c(0,QualityMed,QualityHigh)[QualityType] * gVariables[Name==variable,Quality_Weight]
               TotalQualityScore <- TotalQualityScore +                QualityHigh * gVariables[Name==variable,Quality_Weight] * ( CompleteType >= 2 )
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
               variable <- gVariables[i,Name]
            
               CompleteType <- plotRect(data,year,variable,startx,starty,C,R)
               
               CompletedScore      <- CompletedScore      + c(0,CompleteMed,CompleteHigh)[CompleteType] * gVariables[Name==variable,Completeness_Weight]
               TotalCompletedScore <- TotalCompletedScore +                 CompleteHigh * gVariables[Name==variable,Completeness_Weight]
               
               submittedN <- submittedN + 1 * ( CompleteType >= 2 )
               
               QualityType <- plotCircle(data,year,year_compare,variable,startx,starty,C,R)
               
               QualityScore      <- QualityScore      + c(0,QualityMed,QualityHigh)[QualityType] * gVariables[Name==variable,Quality_Weight]
               TotalQualityScore <- TotalQualityScore +               QualityHigh * gVariables[Name==variable,Quality_Weight] * ( CompleteType >= 2 )
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
               variable <- gVariables[i,Name]
            
               CompleteType <- plotRect(data,year,variable,startx,starty,C,R)
               
               CompletedScore      <- CompletedScore      + c(0,CompleteMed,CompleteHigh)[CompleteType] * gVariables[Name==variable,Completeness_Weight]
               TotalCompletedScore <- TotalCompletedScore +                 CompleteHigh * gVariables[Name==variable,Completeness_Weight]
               
               submittedN <- submittedN + 1 * ( CompleteType >= 2 )
               
               QualityType <- plotCircle(data,year,year_compare,variable,startx,starty,C,R)
               
               QualityScore      <- QualityScore      + c(0,QualityMed,QualityHigh)[QualityType] * gVariables[Name==variable,Quality_Weight]
               TotalQualityScore <- TotalQualityScore +                QualityHigh * gVariables[Name==variable,Quality_Weight] * ( CompleteType >= 2 )
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
               variable <- gVariables[i,Name]
            
               CompleteType <- plotRect(data,year,variable,startx,starty,C,R)
               
               CompletedScore      <- CompletedScore      + c(0,CompleteMed,CompleteHigh)[CompleteType] * gVariables[Name==variable,Completeness_Weight]
               TotalCompletedScore <- TotalCompletedScore +                 CompleteHigh * gVariables[Name==variable,Completeness_Weight]
               
               submittedN <- submittedN + 1 * ( CompleteType >= 2 )
               
               QualityType <- plotCircle(data,year,year_compare,variable,startx,starty,C,R)
               
               QualityScore      <- QualityScore      + c(0,QualityMed,QualityHigh)[QualityType] * gVariables[Name==variable,Quality_Weight]
               TotalQualityScore <- TotalQualityScore +               QualityHigh * gVariables[Name==variable,Quality_Weight] * ( CompleteType >= 2 )
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
     
     grid.rect(x=0.37+xshift+0.01,
                         y=0.015,
                         width=unit(0.007,"npc"),
                         height=unit(0.0125,"npc"),
                         gp=gpar(fill="slategray",col="slategray"))
     
     grid.rect(x=0.46+xshift+0.01,
                         y=0.015,
                         width=unit(0.007,"npc"),
                         height=unit(0.0125,"npc"),
                         gp=gpar(fill="gray75",col="slategray"))
     
     grid.rect(x=0.55+xshift+0.01,
                         y=0.015,
                         width=unit(0.007,"npc"),
                         height=unit(0.0125,"npc"),
                         gp=gpar(fill="white",col="slategray"))
     
     grid.text("Submitted and Complete",x=0.38+xshift+0.01, y=0.015,hjust=0,gp=gpar(col="slategray",fontface="italic",fontsize=6))
     grid.text("Submitted and Incomplete",x=0.47+xshift+0.01, y=0.015,hjust=0,gp=gpar(col="slategray",fontface="italic",fontsize=6))
     grid.text("Not Submitted",x=0.56+xshift+0.01, y=0.015,hjust=0,gp=gpar(col="slategray",fontface="italic",fontsize=6))
     
  
     # high quality   
     grid.circle(x=0.77+xshift,y=0.015,r=unit(0.007,"npc"),gp=gpar(fill="slategray",col="slategray"))

     # medium quality
     grid.circle(x=0.82+xshift,y=0.015,r=unit(0.007,"npc"),gp=gpar(fill="gray75",col="slategray"))
  
     # lowquality
     grid.circle(x=0.87+xshift,y=0.015,r=unit(0.007,"npc"),gp=gpar(fill="white",col="slategray"))
 
     grid.text("High",x=0.78+xshift, y=0.015,hjust=0,gp=gpar(col="slategray",fontface="italic",fontsize=6))
     grid.text("Medium",x=0.83+xshift, y=0.015,hjust=0,gp=gpar(col="slategray",fontface="italic",fontsize=6))
     grid.text("Low",x=0.88+xshift, y=0.015,hjust=0,gp=gpar(col="slategray",fontface="italic",fontsize=6))
     
     
     #grid.text("Not Submitted but Worth Exploring",x=0.805+xshift, y=0.015,hjust=0,gp=gpar(col="slategray",fontface="italic",fontsize=6))
     
     grid.text("Key to data item status and completeness: ",x=0.365+xshift+0.01, y=0.015,hjust=1,gp=gpar(col="steelblue4",fontface="bold",fontsize=8))
     
     grid.text("Key to data item quality: ",x=0.75+xshift, y=0.015,hjust=1,gp=gpar(col="steelblue4",fontface="bold",fontsize=8))
     
     # logos
     grid.raster(image=gLogo,x = 0.03, y=0.85,just = "left", width = 0.10)
     grid.raster(image=gLogo2,x = 0.02, y=0.04,just = "left", width = 0.10)

     # summary section of the report
     
     #grid.text("Overall Score",x=0.43,y=0.925,just="centre",gp=gpar(fontsize=12, fontface="bold", col="slategray"))
     #grid.text("85",x=0.43,y=0.865,just="centre",gp=gpar(fontsize=50, fontface="bold", col="black"))

     

     #grid.text(paste0(getTimelinessScore(state,year),"/10"),x=0.47,y=0.915,just="left",gp=gpar(fontsize=14, fontface="bold", col="steelblue4"))
     #grid.text(paste0(round(10*CompletedScore/TotalCompletedScore,1),"/10"),x=0.47,y=0.87 ,just="left",gp=gpar(fontsize=14, fontface="bold", col="steelblue4"))
     #grid.text(paste0(round(10*QualityScore/TotalQualityScore,1),"/10"),x=0.47,y=0.825,just="left",gp=gpar(fontsize=14, fontface="bold", col="steelblue4"))

    
     tscore <- timetotal*getTimelinessScore(state,year)
     cscore <- round(completetotal*CompletedScore/TotalCompletedScore,1)
     qscore <- round(qualitytotal*QualityScore/TotalQualityScore,1)
     
     scores <- data.table(
                      type=c("Timeliness","Completeness","Quality"),
                      score=c(tscore,cscore,qscore)
                     )

     if(tscore>0)
     {
        grid.rect(x=0.41,y=0.85,
                         width=unit(0.025,"npc"),
                         height=unit(0.1*tscore/max(timetotal,completetotal,qualitytotal),"npc"),vjust =0,
                         gp=gpar(fill="black",col="black"))
     }
     if(cscore>0)
     {
        grid.rect(x=0.475,y=0.85,
                         width=unit(0.025,"npc"),
                         height=unit(0.1*cscore/max(timetotal,completetotal,qualitytotal),"npc"),vjust =0,
                         gp=gpar(fill="black",col="black"))
     }
     if(qscore>0)
     {
        grid.rect(x=0.54,y=0.85,
                         width=unit(0.025,"npc"),
                         height=unit(0.1*qscore/max(timetotal,completetotal,qualitytotal),"npc"),vjust =0,
                         gp=gpar(fill="black",col="black"))
     }
     
     
     grid.text("Timeliness",  x=0.41, y=0.83,hjust=0.5,gp=gpar(fontsize=7, col="gray50"))
     grid.text("Completeness",x=0.475,y=0.83,hjust=0.5,gp=gpar(fontsize=7, col="gray50"))
     grid.text("Quality",     x=0.54, y=0.83,hjust=0.5,gp=gpar(fontsize=7, col="gray50"))

     grid.text(paste0("out of ",timetotal),  x=0.41, y=0.81,hjust=0.5,gp=gpar(fontsize=7, col="gray50"))
     grid.text(paste0("out of ",completetotal),x=0.475,y=0.81,hjust=0.5,gp=gpar(fontsize=7, col="gray50"))
     grid.text(paste0("out of ",qualitytotal),     x=0.54,  y=0.81,hjust=0.5,gp=gpar(fontsize=7, col="gray50"))

          
     grid.text(tscore,x=0.41, y=0.83+0.1*tscore/max(timetotal,completetotal,qualitytotal)+0.03,hjust=0.5,gp=gpar(fontsize=7, col="black"))
     grid.text(cscore,x=0.475,y=0.83+0.1*cscore/max(timetotal,completetotal,qualitytotal)+0.03,hjust=0.5,gp=gpar(fontsize=7, col="black"))
     grid.text(qscore,x=0.54, y=0.83+0.1*qscore/max(timetotal,completetotal,qualitytotal)+0.03,hjust=0.5,gp=gpar(fontsize=7, col="black"))
     
     grid.text("The Score is the sum of
points received from
timeliness, completeness,
and quality.",y=0.90,x=0.71,hjust=1,gp=gpar(fontsize=10, col="gray50"))
     
     grid.circle(x=0.32,y=0.88,r=unit(0.07,"npc"),gp=gpar(fill="gray85",col="gray50"))
     #grid.text("Overall",     x=0.31,y=0.87,just="right",gp=gpar(fontsize=18, col="slategray"))
     grid.text(paste0(tscore+qscore+cscore),x=0.32,y=0.90,just="left",gp=gpar(fontsize=23, col="black"),hjust=0.5)
     grid.text(paste0("out of ",timetotal+completetotal+qualitytotal),x=0.32,y=0.86,just="left",gp=gpar(fontsize=13, col="gray50"),hjust=0.5)
          
     results <- create_overall_report(data,state,year)

     #results <- data.frame(results)
          
     grid.draw(linesGrob(x = unit(c(0.73, 0.73), "npc"),
                         y = unit(c(1,0.79), "npc"),
                         gp=gpar(col="white",lty=1,lwd=3)))
          
     grid.text(paste0("",year  ),x=0.890,y=0.93,gp=gpar(fontsize=9, fontface="bold", col="slategray"),hjust=1)
     grid.text(paste0("",year-1),x=0.950,y=0.93,gp=gpar(fontsize=9, fontface="bold", col="slategray"),hjust=1)
     #grid.text(paste0("Year ",year-2),x=0.780,y=0.925,gp=gpar(fontsize=9, fontface="bold", col="slategray"),hjust=1)
     #grid.text(paste0("Year ",year-3),x=0.840,y=0.925,gp=gpar(fontsize=9, fontface="bold", col="slategray"),hjust=1)
     #grid.text(paste0("Year ",year-4),x=0.900,y=0.925,gp=gpar(fontsize=9, fontface="bold", col="slategray"),hjust=1)
     
     grid.text(results[2,1,with=FALSE],x=0.835,y=0.905,gp=gpar(fontsize=8, fontface="bold", col="slategray"),hjust=1)
     grid.text(results[3,1,with=FALSE],x=0.835,y=0.885,gp=gpar(fontsize=8, fontface="bold", col="slategray"),hjust=1)
     grid.text(results[4,1,with=FALSE],x=0.835,y=0.865,gp=gpar(fontsize=8, fontface="bold", col="slategray"),hjust=1)
     grid.text(results[5,1,with=FALSE],x=0.835,y=0.845,gp=gpar(fontsize=8, fontface="bold", col="slategray"),hjust=1)
     grid.text(results[6,1,with=FALSE],x=0.835,y=0.825,gp=gpar(fontsize=8, fontface="bold", col="slategray"),hjust=1)

     grid.text(results[2,2,with=FALSE],x=0.89,y=0.905,gp=gpar(fontsize=8, col="black"),hjust=1)
     grid.text(results[3,2,with=FALSE],x=0.89,y=0.885,gp=gpar(fontsize=8, col="black"),hjust=1)
     grid.text(results[4,2,with=FALSE],x=0.89,y=0.865,gp=gpar(fontsize=8, col="black"),hjust=1)
     grid.text(results[5,2,with=FALSE],x=0.89,y=0.845,gp=gpar(fontsize=8, col="black"),hjust=1)
     grid.text(results[6,2,with=FALSE],x=0.89,y=0.825,gp=gpar(fontsize=8, col="black"),hjust=1)
     
     grid.text(results[2,3,with=FALSE],x=0.95,y=0.905,gp=gpar(fontsize=8, col="black"),hjust=1)
     grid.text(results[3,3,with=FALSE],x=0.95,y=0.885,gp=gpar(fontsize=8, col="black"),hjust=1)
     grid.text(results[4,3,with=FALSE],x=0.95,y=0.865,gp=gpar(fontsize=8, col="black"),hjust=1)
     grid.text(results[5,3,with=FALSE],x=0.95,y=0.845,gp=gpar(fontsize=8, col="black"),hjust=1)
     grid.text(results[6,3,with=FALSE],x=0.95,y=0.825,gp=gpar(fontsize=8, col="black"),hjust=1)
     
     grid.text("*Does not include non-NHS locals.",x=0.89,y=0.805,gp=gpar(fontsize=5, fontface="italic", col="slategray"),hjust=0.5)
     
     #grid.text(results[1,4,with=FALSE],x=0.780,y=0.900,gp=gpar(fontsize=8, col="black"),hjust=1)
     #grid.text(results[2,4,with=FALSE],x=0.780,y=0.875,gp=gpar(fontsize=8, col="black"),hjust=1)
     #grid.text(results[3,4,with=FALSE],x=0.780,y=0.850,gp=gpar(fontsize=8, col="black"),hjust=1)
     #grid.text(results[4,4,with=FALSE],x=0.780,y=0.825,gp=gpar(fontsize=8, col="black"),hjust=1)
     
     #grid.text(results[1,5,with=FALSE],x=0.840,y=0.900,gp=gpar(fontsize=8, col="black"),hjust=1)
     #grid.text(results[2,5,with=FALSE],x=0.840,y=0.875,gp=gpar(fontsize=8, col="black"),hjust=1)
     #grid.text(results[3,5,with=FALSE],x=0.840,y=0.850,gp=gpar(fontsize=8, col="black"),hjust=1)
     #grid.text(results[4,5,with=FALSE],x=0.840,y=0.825,gp=gpar(fontsize=8, col="black"),hjust=1)
     
     #grid.text(results[1,6,with=FALSE],x=0.900,y=0.900,gp=gpar(fontsize=8, col="black"),hjust=1)
     #grid.text(results[2,6,with=FALSE],x=0.900,y=0.875,gp=gpar(fontsize=8, col="black"),hjust=1)
     #grid.text(results[3,6,with=FALSE],x=0.900,y=0.850,gp=gpar(fontsize=8, col="black"),hjust=1)
     #grid.text(results[4,6,with=FALSE],x=0.900,y=0.825,gp=gpar(fontsize=8, col="black"),hjust=1)
     
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