###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont
#
#
# Description:
#
# Creates the title/first page of the scorecard. During the generation of this page,
# there is a fair amount of analysis performed including applicaiton of the 
# coverage validation and quality recipes.
#
###########################################################################

create_title_page <- function(data, state, year, year_compare=NULL){
  
  scorestotals <- read.table("resources/dat/scoringweights.csv", sep=",", header=TRUE)
  
  timetotal     <- scorestotals[, "timeliness"]
  completetotal <- scorestotals[, "completeness"]
  qualitytotal  <- scorestotals[, "quality"]

  
  # Page setup ==========================================================================
  
  grid.arrange(
    arrangeGrob( 
      rectGrob(gp = gpar(fill = "gray90", col="gray90")),
      rectGrob(gp = gpar(fill = "white", col="white")), 
      rectGrob(gp = gpar(fill = "gray20", col="gray20")), 
      
      arrangeGrob(
        rectGrob(gp = gpar(fill = "gray90", col="gray90")),rectGrob(gp = gpar(fill = "gray90", col="gray90")),rectGrob(gp = gpar(fill = "gray90", col="gray90")),
        rectGrob(gp = gpar(fill = "gray90", col="gray90")), rectGrob(gp = gpar(fill = "gray90", col="gray90")), rectGrob(gp = gpar(fill = "gray90", col="gray90")),
        rectGrob(gp = gpar(fill = "white", col="white")), rectGrob(gp = gpar(fill = "white", col="white")), rectGrob(gp = gpar(fill = "white", col="white")),
        rectGrob(gp = gpar(fill = "white", col="white")), rectGrob(gp = gpar(fill = "white", col="white")), rectGrob(gp = gpar(fill = "white", col="white")),
        rectGrob(gp = gpar(fill = "white", col="white")), rectGrob(gp = gpar(fill = "white", col="white")), rectGrob(gp = gpar(fill = "white", col="white")),
        nrow=5, ncol=3, widths=unit(c(0.035, 0.93, 0.035), units="npc"), heights=unit(c(0.06, 0.15, 0.765, 0.03, 0.005), units="npc")
      ),
      nrow = 1,
      widths=unit(c(0.2, 0.005, 0.005, 0.79), units="npc")
    ),
    nrow=1,
    heights = unit(1, units="npc"))
  
  grid.text("HPMS SCORECARD", 
            x = 0.03, 
            y = 0.68, 
            just = "left", 
            gp = gpar(col = "slategray", fontface = "bold", fontsize = 10)
            )
  
  grid.draw(linesGrob(x = unit(c(0.03, 0.18), "npc"),
                      y = unit(c(0.66, 0.66), "npc"),
                      gp=gpar(col="black", lty=1)))
  
  grid.text(toupper(gState_Labels[index==state, label]), 
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
                      y = unit(c(0.505, 0.505), "npc"),
                      gp=gpar(col="black", lty=1)))
  
  grid.text(paste0("Generated: ", format(Sys.time(), "%B %d, %Y")), 
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

  
  # Add section headers

  sec_header_gp <- gpar(col = "steelblue4", fontface = "bold", fontsize = 13)
  line_gp <- gpar(col='slategray', lty=3)
  
  hdr_left <- 0.25
  y_top_hdr <- 0.98
  
  grid.text("Score"           , hdr_left, y_top_hdr, hjust=0, gp=sec_header_gp)
  grid.text("inventory"       , hdr_left, 0.823-vertical_adj, hjust=0, gp=sec_header_gp)
  grid.text("pavement"        , hdr_left, 0.665-vertical_adj, hjust=0, gp=sec_header_gp)
  grid.text("traffic"         , hdr_left, 0.535-vertical_adj, hjust=0, gp=sec_header_gp)
  grid.text("geometric"       , hdr_left, 0.387-vertical_adj, hjust=0, gp=sec_header_gp)
  grid.text("route"           , hdr_left, 0.237-vertical_adj, hjust=0, gp=sec_header_gp)
  grid.text("special networks", hdr_left, 0.14-vertical_adj, hjust=0, gp=sec_header_gp)
  
  # inventory
  grid.draw(linesGrob(x = unit(c(hdr_left, 0.97), "npc"),
                      y = unit(c(0.8045, 0.8045)-vertical_adj, "npc"),
                      gp=line_gp))
  
  # pavement
  grid.draw(linesGrob(x = unit(c(hdr_left, 0.97), "npc"),
                      y = unit(c(0.6465, 0.6465)-vertical_adj, "npc"),
                      gp=line_gp))
  
  # traffic
  grid.draw(linesGrob(x = unit(c(hdr_left, 0.97), "npc"),
                      y = unit(c(0.5165, 0.5165)-vertical_adj, "npc"),
                      gp=line_gp))
  
  # geometric
  grid.draw(linesGrob(x = unit(c(hdr_left, 0.97), "npc"),
                      y = unit(c(0.3685, 0.3685)-vertical_adj, "npc"),
                      gp=line_gp))
  
  # route
  grid.draw(linesGrob(x = unit(c(hdr_left, 0.97), "npc"),
                      y = unit(c(0.2185, 0.2185)-vertical_adj, "npc"),
                      gp=line_gp))
  
  # special network
  grid.draw(linesGrob(x = unit(c(hdr_left, 0.97), "npc"),
                      y = unit(c(0.12, 0.12)-vertical_adj, "npc"),
                      gp=line_gp))


  
  # Data summary -------------------------------------------------------------------------
  # Add the data summary table
  
  results <- create_data_summary(data, state, year, year_compare)
  results <- results[nrow(results):1,]
  #results <- data.frame(results)
  
  grid.draw(linesGrob(x = unit(c(0.73, 0.73), "npc"), 
                      y = unit(c(1, 0.79), "npc"), 
                      gp=gpar(col="white", lty=1, lwd=3)))

  label_gp <- gpar(fontsize=8, fontface="bold", col="slategray")
  item_gp <- gpar(fontsize=8, col="black")

  y_bot <- 0.825

  x_label <- 0.85
  x_col1 <- x_label + 0.055
  x_col2 <- x_col1 + 0.055
  y_row_spc <- 0.02
  
  grid.text("Data Summary"    , x_label, y_top_hdr, hjust=1, gp=sec_header_gp)

  grid.text(paste0("", year  ),
            x=x_col1,
            y=y_bot + y_row_spc * (nrow(results) - 1) + 0.005,
            gp=gpar(fontsize=9, fontface="bold", col="slategray"),
            hjust=1)
  
  grid.text(paste0("", year_compare),
            x=x_col2,
            y=y_bot + y_row_spc * (nrow(results) - 1) + 0.005,
            gp=gpar(fontsize=9, fontface="bold", col="slategray"),
            hjust=1)

  # Draw labels
  for ( i in 1:(nrow(results) - 1) ){
    grid.text(results[i, 1, with=FALSE], x=x_label, y=y_bot + y_row_spc*(i-1), gp=label_gp, hjust=1)
    grid.text(results[i, 2, with=FALSE], x=x_col1, y=y_bot + y_row_spc*(i-1), gp=item_gp, hjust=1)
    grid.text(results[i, 3, with=FALSE], x=x_col2, y=y_bot + y_row_spc*(i-1), gp=item_gp, hjust=1)

  }    

  grid.text("*Does not include non-NHS locals.", x=x_col1, y=y_bot - y_row_spc,
            gp=gpar(fontsize=5, fontface="italic", col="slategray"), hjust=0.5)


  
  # Completeness and quality ==========================================================
  # The following section calculates completeness and quality for each data item
  # Then plots it.
  
  cat("\nCalculating coverage validation results. This may take some time to complete.")
  
  
  # Inventory --------------------------------------------------------------
  
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
  
  for(i in 1:length(gVariables[, Name]))
  {
    
    if(gVariables[i, Grouping]=="I")
    {
      grid.draw(textGrob(gVariables[i, Name], x=startx+(C-1)*0.15, starty-(R-1)*rowWidth, hjust=1, gp=gpar(col="slategray", fontsize=7)))
      #grid.ellipse(x=startx+(C-1)*0.15+0.01, y=starty-(R-1)*0.0175, size=2.5, ar=1, angle=0, def="npc", gp=gpar(fill="white", col="slategray"))
      
      variable <- gVariables[i, Name]
      
      CompleteType <- plotRect(data, year, variable, startx, starty, C, R)
      
      CompletedScore      <- CompletedScore      + c(0, CompleteMed, CompleteHigh)[CompleteType] * gVariables[Name==variable, Completeness_Weight]
      TotalCompletedScore <- TotalCompletedScore +                 CompleteHigh * gVariables[Name==variable, Completeness_Weight]
      
      submittedN <- submittedN + 1 * ( CompleteType >= 2 )
      
      QualityType <- plotCircle(data, year, year_compare, variable, startx, starty, C, R)
      
      QualityScore      <- QualityScore      + c(0, QualityMed, QualityHigh)[QualityType] * gVariables[Name==variable, Quality_Weight]
      TotalQualityScore <- TotalQualityScore +                QualityHigh * gVariables[Name==variable, Quality_Weight] * ( CompleteType >= 2 )
      
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
  
  
  # Pavement ---------------------------------------------------------------
  
  R <- 1
  C <- 1
  startx <- 0.35
  starty <- 0.63-vertical_adj
  for(i in 1:length(gVariables[, Name]))
  {
    
    if(gVariables[i, Grouping]=="P")
    {
      grid.draw(textGrob(gVariables[i, Name], x=startx+(C-1)*0.15, starty-(R-1)*rowWidth, hjust=1, gp=gpar(col="slategray", fontsize=7)))
      #grid.ellipse(x=startx+(C-1)*0.15+0.01, y=starty-(R-1)*0.0175, size=2.5, ar=1, angle=0, def="npc", gp=gpar(fill="white", col="red"))
      variable <- gVariables[i, Name]
      
      CompleteType <- plotRect(data, year, variable, startx, starty, C, R)
      
      CompletedScore      <- CompletedScore      + c(0, CompleteMed, CompleteHigh)[CompleteType] * gVariables[Name==variable, Completeness_Weight]
      TotalCompletedScore <- TotalCompletedScore +                 CompleteHigh * gVariables[Name==variable, Completeness_Weight]
      
      submittedN <- submittedN + 1 * ( CompleteType >= 2 )
      
      QualityType <- plotCircle(data, year, year_compare, variable, startx, starty, C, R)
      
      QualityScore      <- QualityScore      + c(0, QualityMed, QualityHigh)[QualityType] * gVariables[Name==variable, Quality_Weight]
      TotalQualityScore <- TotalQualityScore +                QualityHigh * gVariables[Name==variable, Quality_Weight] * ( CompleteType >= 2 )
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
  
  # Traffic ----------------------------------------------------------------
  
  R <- 1
  C <- 1
  startx <- 0.35
  starty <- 0.5-vertical_adj
  for(i in 1:length(gVariables[, Name]))
  {
    
    if(gVariables[i, Grouping]=="T")
    {
      grid.draw(textGrob(gVariables[i, Name], x=startx+(C-1)*0.15, starty-(R-1)*rowWidth, hjust=1, gp=gpar(col="slategray", fontsize=7)))
      #grid.ellipse(x=startx+(C-1)*0.15+0.01, y=starty-(R-1)*0.0175, size=2.5, ar=1, angle=0, def="npc", gp=gpar(fill="red", col="red"))
      variable <- gVariables[i, Name]
      
      CompleteType <- plotRect(data, year, variable, startx, starty, C, R)
      
      CompletedScore      <- CompletedScore      + c(0, CompleteMed, CompleteHigh)[CompleteType] * gVariables[Name==variable, Completeness_Weight]
      TotalCompletedScore <- TotalCompletedScore +                 CompleteHigh * gVariables[Name==variable, Completeness_Weight]
      
      submittedN <- submittedN + 1 * ( CompleteType >= 2 )
      
      QualityType <- plotCircle(data, year, year_compare, variable, startx, starty, C, R)
      
      QualityScore      <- QualityScore      + c(0, QualityMed, QualityHigh)[QualityType] * gVariables[Name==variable, Quality_Weight]
      TotalQualityScore <- TotalQualityScore +                QualityHigh * gVariables[Name==variable, Quality_Weight] * ( CompleteType >= 2 )
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
  
  
  # Geometric -------------------------------------------------------------
  
  R <- 1
  C <- 1
  startx <- 0.35
  starty <- 0.35-vertical_adj
  for(i in 1:length(gVariables[, Name]))
  {
    
    if(gVariables[i, Grouping]=="G")
    {
      grid.draw(textGrob(gVariables[i, Name], x=startx+(C-1)*0.15, starty-(R-1)*rowWidth, hjust=1, gp=gpar(col="slategray", fontsize=7)))
      #grid.ellipse(x=startx+(C-1)*0.15+0.01, y=starty-(R-1)*0.0175, size=2.5, ar=1, angle=0, def="npc", gp=gpar(fill="slategray", col="slategray"))
      variable <- gVariables[i, Name]
      
      CompleteType <- plotRect(data, year, variable, startx, starty, C, R)
      
      CompletedScore      <- CompletedScore      + c(0, CompleteMed, CompleteHigh)[CompleteType] * gVariables[Name==variable, Completeness_Weight]
      TotalCompletedScore <- TotalCompletedScore +                 CompleteHigh * gVariables[Name==variable, Completeness_Weight]
      
      submittedN <- submittedN + 1 * ( CompleteType >= 2 )
      
      QualityType <- plotCircle(data, year, year_compare, variable, startx, starty, C, R)
      
      QualityScore      <- QualityScore      + c(0, QualityMed, QualityHigh)[QualityType] * gVariables[Name==variable, Quality_Weight]
      TotalQualityScore <- TotalQualityScore +               QualityHigh * gVariables[Name==variable, Quality_Weight] * ( CompleteType >= 2 )
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
  
  
  # ROute -----------------------------------------------------------------
  
  R <- 1
  C <- 1
  startx <- 0.35
  starty <- 0.2-vertical_adj
  for(i in 1:length(gVariables[, Name]))
  {
    
    if(gVariables[i, Grouping]=="R")
    {
      grid.draw(textGrob(gVariables[i, Name], x=startx+(C-1)*0.15, starty-(R-1)*rowWidth, hjust=1, gp=gpar(col="slategray", fontsize=7)))
      #grid.ellipse(x=startx+(C-1)*0.15+0.01, y=starty-(R-1)*0.0175, size=2.5, ar=1, angle=0, def="npc", gp=gpar(fill="red", col="red"))
      variable <- gVariables[i, Name]
      
      CompleteType <- plotRect(data, year, variable, startx, starty, C, R)
      
      CompletedScore      <- CompletedScore      + c(0, CompleteMed, CompleteHigh)[CompleteType] * gVariables[Name==variable, Completeness_Weight]
      TotalCompletedScore <- TotalCompletedScore +                 CompleteHigh * gVariables[Name==variable, Completeness_Weight]
      
      submittedN <- submittedN + 1 * ( CompleteType >= 2 )
      
      QualityType <- plotCircle(data, year, year_compare, variable, startx, starty, C, R)
      
      QualityScore      <- QualityScore      + c(0, QualityMed, QualityHigh)[QualityType] * gVariables[Name==variable, Quality_Weight]
      TotalQualityScore <- TotalQualityScore +                QualityHigh * gVariables[Name==variable, Quality_Weight] * ( CompleteType >= 2 )
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
  
  
  # special networks -------------------------------------------------------
  
  R <- 1
  C <- 1
  startx <- 0.35
  starty <- 0.105-vertical_adj
  for(i in 1:length(gVariables[, Name]))
  {
    
    if(gVariables[i, Grouping]=="SN")
    {
      grid.draw(textGrob(gVariables[i, Name], x=startx+(C-1)*0.15, starty-(R-1)*rowWidth, hjust=1, gp=gpar(col="slategray", fontsize=7)))
      #grid.ellipse(x=startx+(C-1)*0.15+0.01, y=starty-(R-1)*0.0175, size=2.5, ar=1, angle=0, def="npc", gp=gpar(fill="slategray", col="slategray"))
      variable <- gVariables[i, Name]
      
      CompleteType <- plotRect(data, year, variable, startx, starty, C, R)
      
      CompletedScore      <- CompletedScore      + c(0, CompleteMed, CompleteHigh)[CompleteType] * gVariables[Name==variable, Completeness_Weight]
      TotalCompletedScore <- TotalCompletedScore +                 CompleteHigh * gVariables[Name==variable, Completeness_Weight]
      
      submittedN <- submittedN + 1 * ( CompleteType >= 2 )
      
      QualityType <- plotCircle(data, year, year_compare, variable, startx, starty, C, R)
      
      QualityScore      <- QualityScore      + c(0, QualityMed, QualityHigh)[QualityType] * gVariables[Name==variable, Quality_Weight]
      TotalQualityScore <- TotalQualityScore +               QualityHigh * gVariables[Name==variable, Quality_Weight] * ( CompleteType >= 2 )
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
  
  
  # legend ----------------------------------------------------------------
  
  grid.draw(linesGrob(x = unit(c(0.25, 0.97), "npc"),
                      y = unit(c(0.03, 0.03), "npc"),
                      gp=gpar(col="gray60")))
  
  xshift <- 0.05
  
  grid.rect(x=0.37+xshift+0.01,
            y=0.015,
            width=unit(0.007, "npc"), 
            height=unit(0.0125, "npc"), 
            gp=gpar(fill="slategray", col="slategray"))
  
  grid.rect(x=0.46+xshift+0.01, 
            y=0.015, 
            width=unit(0.007, "npc"), 
            height=unit(0.0125, "npc"), 
            gp=gpar(fill="gray75", col="slategray"))
  
  grid.rect(x=0.55+xshift+0.01, 
            y=0.015, 
            width=unit(0.007, "npc"), 
            height=unit(0.0125, "npc"), 
            gp=gpar(fill="white", col="slategray"))
  
  grid.text("Submitted and Complete", x=0.38+xshift+0.01, y=0.015, hjust=0, gp=gpar(col="slategray", fontface="italic", fontsize=6))
  grid.text("Submitted and Incomplete", x=0.47+xshift+0.01, y=0.015, hjust=0, gp=gpar(col="slategray", fontface="italic", fontsize=6))
  grid.text("Not Submitted", x=0.56+xshift+0.01, y=0.015, hjust=0, gp=gpar(col="slategray", fontface="italic", fontsize=6))
  
  
  # high quality   
  grid.circle(x=0.77+xshift, y=0.015, r=unit(0.007, "npc"), gp=gpar(fill="slategray", col="slategray"))

  # medium quality
  grid.circle(x=0.82+xshift, y=0.015, r=unit(0.007, "npc"), gp=gpar(fill="gray75", col="slategray"))
  
  # lowquality
  grid.circle(x=0.87+xshift, y=0.015, r=unit(0.007, "npc"), gp=gpar(fill="white", col="slategray"))
  
  grid.text("High", x=0.78+xshift, y=0.015, hjust=0, gp=gpar(col="slategray", fontface="italic", fontsize=6))
  grid.text("Medium", x=0.83+xshift, y=0.015, hjust=0, gp=gpar(col="slategray", fontface="italic", fontsize=6))
  grid.text("Low", x=0.88+xshift, y=0.015, hjust=0, gp=gpar(col="slategray", fontface="italic", fontsize=6))
  
  
  #grid.text("Not Submitted but Worth Exploring", x=0.805+xshift, y=0.015, hjust=0, gp=gpar(col="slategray", fontface="italic", fontsize=6))
  
  grid.text("Key to data item status and completeness: ", x=0.365+xshift+0.01, y=0.015, hjust=1, gp=gpar(col="steelblue4", fontface="bold", fontsize=8))
  
  grid.text("Key to data item quality: ", x=0.75+xshift, y=0.015, hjust=1, gp=gpar(col="steelblue4", fontface="bold", fontsize=8))
  
  # logos
  grid.raster(image=gLogo, x = 0.03, y=0.85, just = "left", width = 0.10)
  grid.raster(image=gLogo2, x = 0.02, y=0.04, just = "left", width = 0.10)

  
  # Summary ============================================================================

  # summary section of the report 

  # Completeness, quality, and timeliness scores --------------------------------------
  
  tscore <- timetotal*getTimelinessScore(state, year)
  cscore <- round(completetotal*CompletedScore/TotalCompletedScore, 1)
  qscore <- round(qualitytotal*QualityScore/TotalQualityScore, 1)
  
  scores <- data.table(
    type=c("Timeliness", "Completeness", "Quality"), 
    score=c(tscore, cscore, qscore)
  )

  if(tscore>0)
  {
    grid.rect(x=0.41, y=0.85, 
              width=unit(0.025, "npc"), 
              height=unit(0.1*tscore/max(timetotal, completetotal, qualitytotal), "npc"), vjust =0, 
              gp=gpar(fill="black", col="black"))
  }
  if(cscore>0)
  {
    grid.rect(x=0.475, y=0.85, 
              width=unit(0.025, "npc"), 
              height=unit(0.1*cscore/max(timetotal, completetotal, qualitytotal), "npc"), vjust =0, 
              gp=gpar(fill="black", col="black"))
  }
  if(qscore>0)
  {
    grid.rect(x=0.54, y=0.85, 
              width=unit(0.025, "npc"), 
              height=unit(0.1*qscore/max(timetotal, completetotal, qualitytotal), "npc"), vjust =0, 
              gp=gpar(fill="black", col="black"))
  }
  
  
  grid.text("Timeliness",  x=0.41, y=0.83, hjust=0.5, gp=gpar(fontsize=7, col="gray50"))
  grid.text("Completeness", x=0.475, y=0.83, hjust=0.5, gp=gpar(fontsize=7, col="gray50"))
  grid.text("Quality",     x=0.54, y=0.83, hjust=0.5, gp=gpar(fontsize=7, col="gray50"))

  grid.text(paste0("out of ", timetotal),  x=0.41, y=0.81, hjust=0.5, gp=gpar(fontsize=7, col="gray50"))
  grid.text(paste0("out of ", completetotal), x=0.475, y=0.81, hjust=0.5, gp=gpar(fontsize=7, col="gray50"))
  grid.text(paste0("out of ", qualitytotal),     x=0.54,  y=0.81, hjust=0.5, gp=gpar(fontsize=7, col="gray50"))

  
  grid.text(tscore, x=0.41, y=0.83+0.1*tscore/max(timetotal, completetotal, qualitytotal)+0.03, hjust=0.5, gp=gpar(fontsize=7, col="black"))
  grid.text(cscore, x=0.475, y=0.83+0.1*cscore/max(timetotal, completetotal, qualitytotal)+0.03, hjust=0.5, gp=gpar(fontsize=7, col="black"))
  grid.text(qscore, x=0.54, y=0.83+0.1*qscore/max(timetotal, completetotal, qualitytotal)+0.03, hjust=0.5, gp=gpar(fontsize=7, col="black"))
  
  grid.text("The Score is the sum of\npoints received from\ntimeliness, completeness, \nand quality.", 
            y=0.90, x=0.71, hjust=1, gp=gpar(fontsize=10, col="gray50"))
  
  grid.circle(x=0.32, y=0.88, r=unit(0.07, "npc"), gp=gpar(fill="gray85", col="gray50"))
  #grid.text("Overall",     x=0.31, y=0.87, just="right", gp=gpar(fontsize=18, col="slategray"))
  grid.text(paste0(tscore+qscore+cscore), x=0.32, y=0.90, just="left", gp=gpar(fontsize=23, col="black"), hjust=0.5)
  grid.text(paste0("out of ", timetotal+completetotal+qualitytotal), x=0.32, y=0.86, just="left", gp=gpar(fontsize=13, col="gray50"), hjust=0.5)
  
}
