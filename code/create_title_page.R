###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont and Matt Landis
#
#
# Description:
#
# Creates the title/first page of the scorecard. During the generation of this page,
# there is a fair amount of analysis performed including calculation of the 
# completeness and quality scores.
#
###########################################################################

create_title_page <- function(data, state, year, year_compare = NULL) {
  
  scorestotals <- read.table("resources/dat/scoringweights.csv",
                             sep = ",",
                             header = TRUE)
  
  time_weight     <- scorestotals[, "timeliness"]
  complete_weight <- scorestotals[, "completeness"]
  quality_weight  <- scorestotals[, "quality"]
  cvWeight        <- scorestotals[, 'cross_validation']
  
  # Page setup ==============================================================
  
  grid.arrange(
    arrangeGrob(
      rectGrob(gp = gpar(fill = "gray90", col = "gray90")),
      rectGrob(gp = gpar(fill = "white", col = "white")),
      rectGrob(gp = gpar(fill = "gray20", col = "gray20")),
      
      arrangeGrob(
        rectGrob(gp = gpar(fill = "gray90", col = "gray90")),
        rectGrob(gp = gpar(fill = "gray90", col = "gray90")),
        rectGrob(gp = gpar(fill = "gray90", col = "gray90")),
        rectGrob(gp = gpar(fill = "gray90", col = "gray90")),
        rectGrob(gp = gpar(fill = "gray90", col = "gray90")),
        rectGrob(gp = gpar(fill = "gray90", col = "gray90")),
        rectGrob(gp = gpar(fill = "white", col = "white")),
        rectGrob(gp = gpar(fill = "white", col = "white")),
        rectGrob(gp = gpar(fill = "white", col = "white")),
        rectGrob(gp = gpar(fill = "white", col = "white")),
        rectGrob(gp = gpar(fill = "white", col = "white")),
        rectGrob(gp = gpar(fill = "white", col = "white")),
        rectGrob(gp = gpar(fill = "white", col = "white")),
        rectGrob(gp = gpar(fill = "white", col = "white")),
        rectGrob(gp = gpar(fill = "white", col = "white")),
        nrow = 5,
        ncol = 3,
        widths = unit(c(0.035, 0.93, 0.035), units = "npc"),
        heights = unit(c(0.06, 0.15, 0.765, 0.03, 0.005), units = "npc")
      ),
      nrow = 1,
      widths = unit(c(0.2, 0.005, 0.005, 0.79), units = "npc")
    ),
    nrow = 1,
    heights = unit(1, units = "npc")
  )
  
  grid.text(
    "HPMS SCORECARD",
    x = 0.03,
    y = 0.68,
    just = "left",
    gp = gpar(
      col = "slategray",
      fontface = "bold",
      fontsize = 10
    )
  )
  
  grid.draw(linesGrob(
    x = unit(c(0.03, 0.18), "npc"),
    y = unit(c(0.66, 0.66), "npc"),
    gp = gpar(col = "black", lty = 1)
  ))
  
  grid.text(
    toupper(gState_Labels[index == state, label]),
    x = 0.03,
    y = 0.63,
    just = "left",
    gp = gpar(
      col = "black",
      fontface = "bold",
      fontsize = 13
    )
  )
  
  grid.text(
    year,
    x = 0.03,
    y = 0.57,
    just = "left",
    gp = gpar(
      col = "black",
      fontface = "bold",
      fontsize = 27
    )
  )
  
  grid.draw(linesGrob(
    x = unit(c(0.03, 0.18), "npc"),
    y = unit(c(0.505, 0.505), "npc"),
    gp = gpar(col = "black", lty = 1)
  ))
  
  grid.text(
    paste0("Generated: ", format(Sys.time(), "%B %d, %Y")),
    x = 0.03,
    y = 0.52,
    just = "left",
    gp = gpar(
      col = "black",
      fontface = "italic",
      fontsize = 7
    )
  )
  
  grid.text(
    title_text,
    x = 0.03,
    y = 0.285,
    just = "left",
    gp = gpar(col = "black", fontsize = 6)
  )
  
  
  
  vertical_adj <- 0.05
  
  
  # Add section headers
  
  sec_header_gp <-
    gpar(col = "steelblue4",
         fontface = "bold",
         fontsize = 13)
  line_gp <- gpar(col = 'slategray', lty = 3)
  
  hdr_left <- 0.25
  y_top_hdr <- 0.98
  
  grid.text("Score"           ,
            hdr_left,
            y_top_hdr,
            hjust = 0,
            gp = sec_header_gp)
  grid.text("inventory"       ,
            hdr_left,
            0.823 - vertical_adj,
            hjust = 0,
            gp = sec_header_gp)
  grid.text("pavement"        ,
            hdr_left,
            0.665 - vertical_adj,
            hjust = 0,
            gp = sec_header_gp)
  grid.text("traffic"         ,
            hdr_left,
            0.535 - vertical_adj,
            hjust = 0,
            gp = sec_header_gp)
  grid.text("geometric"       ,
            hdr_left,
            0.387 - vertical_adj,
            hjust = 0,
            gp = sec_header_gp)
  grid.text("route"           ,
            hdr_left,
            0.237 - vertical_adj,
            hjust = 0,
            gp = sec_header_gp)
  grid.text("special networks",
            hdr_left,
            0.14 - vertical_adj,
            hjust = 0,
            gp = sec_header_gp)
  
  # inventory
  grid.draw(linesGrob(
    x = unit(c(hdr_left, 0.97), "npc"),
    y = unit(c(0.8045, 0.8045) - vertical_adj, "npc"),
    gp = line_gp
  ))
  
  # pavement
  grid.draw(linesGrob(
    x = unit(c(hdr_left, 0.97), "npc"),
    y = unit(c(0.6465, 0.6465) - vertical_adj, "npc"),
    gp = line_gp
  ))
  
  # traffic
  grid.draw(linesGrob(
    x = unit(c(hdr_left, 0.97), "npc"),
    y = unit(c(0.5165, 0.5165) - vertical_adj, "npc"),
    gp = line_gp
  ))
  
  # geometric
  grid.draw(linesGrob(
    x = unit(c(hdr_left, 0.97), "npc"),
    y = unit(c(0.3685, 0.3685) - vertical_adj, "npc"),
    gp = line_gp
  ))
  
  # route
  grid.draw(linesGrob(
    x = unit(c(hdr_left, 0.97), "npc"),
    y = unit(c(0.2185, 0.2185) - vertical_adj, "npc"),
    gp = line_gp
  ))
  
  # special network
  grid.draw(linesGrob(
    x = unit(c(hdr_left, 0.97), "npc"),
    y = unit(c(0.12, 0.12) - vertical_adj, "npc"),
    gp = line_gp
  ))
  
  
  
  # Data summary -----------------------------------------------------------
  # Add the data summary table
  
  results <- create_data_summary(data, state, year, year_compare)
  results <- results[nrow(results):1,]
  #results <- data.frame(results)
  
  grid.draw(linesGrob(
    x = unit(c(0.73, 0.73), "npc"),
    y = unit(c(1, 0.79), "npc"),
    gp = gpar(col = "white", lty = 1, lwd = 3)
  ))
  
  label_gp <- gpar(fontsize = 8,
                   fontface = "bold",
                   col = "slategray")
  item_gp <- gpar(fontsize = 8, col = "black")
  
  y_bot <- 0.825
  
  x_label <- 0.85
  x_col1 <- x_label + 0.055
  x_col2 <- x_col1 + 0.055
  y_row_spc <- 0.02
  
  grid.text("Data Summary"    ,
            x_label,
            y_top_hdr,
            hjust = 1,
            gp = sec_header_gp)
  
  grid.text(
    paste0("", year),
    x = x_col1,
    y = y_bot + y_row_spc * (nrow(results) - 1) + 0.005,
    gp = gpar(
      fontsize = 9,
      fontface = "bold",
      col = "slategray"
    ),
    hjust = 1
  )
  
  grid.text(
    paste0("", year_compare),
    x = x_col2,
    y = y_bot + y_row_spc * (nrow(results) - 1) + 0.005,
    gp = gpar(
      fontsize = 9,
      fontface = "bold",
      col = "slategray"
    ),
    hjust = 1
  )
  
  # Draw labels
  for (i in 1:(nrow(results) - 1)) {
    grid.text(
      results[i, 1, with = FALSE],
      x = x_label,
      y = y_bot + y_row_spc * (i - 1),
      gp = label_gp,
      hjust = 1
    )
    grid.text(
      results[i, 2, with = FALSE],
      x = x_col1,
      y = y_bot + y_row_spc * (i - 1),
      gp = item_gp,
      hjust = 1
    )
    grid.text(
      results[i, 3, with = FALSE],
      x = x_col2,
      y = y_bot + y_row_spc * (i - 1),
      gp = item_gp,
      hjust = 1
    )
    
  }
  
  grid.text(
    "*Does not include non-NHS locals.",
    x = x_col1,
    y = y_bot - y_row_spc,
    gp = gpar(
      fontsize = 5,
      fontface = "italic",
      col = "slategray"
    ),
    hjust = 0.5
  )
  
  
  # Completeness and quality ================================================
  # The following section calculates completeness and quality for each data item
  # Then plots it.
  
  cat("\nCalculating quality score for each item. This may take some time to complete.\n")
  
  dt_quality <- calcQualityAll(data, year, year_compare)
  dt_cross <- calc_cross_validation(data, year)
  
  colWidth <- 0.152
  rowWidth <- 0.020 
  space1 <- 0.008    # Space between start of text and completeness symbol
  space2 <- 0.008   # Space between completeness symbol and quality symbol
  
  CompletedScore <- 0
  CompletedScoreMax <- 0
  
  #QualityScore <- 0
  submittedN   <- 0
  #QualityScoreMax <- 0
  
  # Scores for getting a medium or high quality value
  CompleteMed  <- 1
  CompleteHigh <- 1.5
  
  # Set parameters for each section
  
  group_params <- data.frame(
    Grouping = c('Inventory', 'Pavement', 'Traffic', 'Geometric', 'Route', 'Special Networks'),
    abbrev   = c( 'I', 'P', 'T', 'G', 'R', 'SN'),
    starty   = c(0.79, 0.63, 0.5, 0.35, 0.2, 0.105),
    nRow     = c(   4,    3,   4,    5,   1,     1))
  
  cat("\nCalculating coverage validation results. This may take some time to complete.")
    # Print completeness and quality for each data item
  for ( g in 1:nrow(group_params)){
    
    R <- 1
    C <- 1
    startx <- 0.32
    starty <- group_params$starty[g] - vertical_adj
    
    group_vars <- dt_quality[Grouping == group_params$abbrev[g]]
    
    for (i in 1:nrow(group_vars)){
      
      variable <- group_vars[i, Name]
      
      grid.draw(textGrob(
        label=variable,
        x=startx + (C - 1) * colWidth,
        y=starty - (R - 1) * rowWidth,
        hjust = 1,
        gp = gpar(col = "slategray", fontsize = 7)
      ))
      
      CompleteType <-
        plotCompleteness(data, year, variable,
                        x = startx + (C - 1) * colWidth + space1,
                        y = starty - (R - 1) * rowWidth)

      CompletedScore <-
        CompletedScore + c(0, CompleteMed, CompleteHigh)[CompleteType] * group_vars$Completeness_Weight[i]

      CompletedScoreMax <-
        CompletedScoreMax + CompleteHigh * group_vars$Completeness_Weight[i]

      submittedN <- submittedN + 1 * (CompleteType >= 2)

      thisQuality <- group_vars$Quality_Score[i] 
      
      plotQuality(thisQuality,
                  x = startx + (C - 1) * colWidth + space1 + space2,
                  y = starty - (R - 1) * rowWidth)
      
      if (R < group_params$nRow[g]){
        R <- R + 1
      } else {
        R <- 1
        C <- 1 + C
      }
    }
  }
  
  QualityScore    <- sum(dt_quality$Quality_Score * dt_quality$Quality_Weight, na.rm=TRUE)
  QualityScoreMax <- sum(!is.na(dt_quality$Quality_Score) * dt_quality$Quality_Weight) * 100
  qMean <- QualityScore / QualityScoreMax
  
  # Incorporate cross-validation score
  cvMean <- mean(dt_cross$mileage_pass, na.rm=TRUE)
  
  QualityCross <- qMean * (1 - cvWeight) + cvMean * cvWeight  
  
  #(cvMean * nrow(dt_cross) + qMean * nrow(dt_quality)) /
  #  (nrow(dt_cross) + nrow(dt_quality))
  
  
  # Write out the quality scores ----------------
  
  path <- file.path('data', getStateLabelFromNum(data$state_code[1]))
  file <- paste0(getStateLabelFromNum(data$state_code[1]), '_', year, '_', year_compare,
                 '_quality_summary.csv')
  fullpath <- file.path(path, file)
  
  if (!dir.exists(path)) dir.create(path)
  
  write.csv(x=dt_quality, file=fullpath, na='', row.names=FALSE)

  # Write out the cross-validation scores ----------------
  path <- file.path('data', getStateLabelFromNum(data$state_code[1]))
  file <- paste0(getStateLabelFromNum(data$state_code[1]), '_', year,
                 '_cross_validation_summary.csv')
  fullpath <- file.path(path, file)
  
  if (!dir.exists(path)) dir.create(path)
  
  write.csv(x=dt_cross, file=fullpath, na='', row.names=FALSE)
  
  
  # legend ----------------------------------------------------------------
  
  grid.draw(linesGrob(
    x = unit(c(0.25, 0.97), "npc"),
    y = unit(c(0.03, 0.03), "npc"),
    gp = gpar(col = "gray60")
  ))
  
  xshift <- 0.05
  
  grid.circle(
    x = 0.37 + xshift + 0.01,
    y = 0.015,
    r = unit(0.007, "npc"),
    gp = gpar(fill = "slategray", col = "slategray")
  )
  
  grid.circle(
    x = 0.46 + xshift + 0.01,
    y = 0.015,
    r = unit(0.007, "npc"),
    gp = gpar(fill = "gray75", col = "slategray")
  )
  
  grid.circle(
    x = 0.55 + xshift + 0.01,
    y = 0.015,
    r = unit(0.007, "npc"),
    gp = gpar(fill = "white", col = "slategray")
  )
  
  grid.text(
    "Submitted and Complete",
    x = 0.38 + xshift + 0.01,
    y = 0.015,
    hjust = 0,
    gp = gpar(
      col = "slategray",
      fontface = "italic",
      fontsize = 6
    )
  )
  
  grid.text(
    "Submitted and Incomplete",
    x = 0.47 + xshift + 0.01,
    y = 0.015,
    hjust = 0,
    gp = gpar(
      col = "slategray",
      fontface = "italic",
      fontsize = 6
    )
  )
  
  grid.text(
    "Not Submitted",
    x = 0.56 + xshift + 0.01,
    y = 0.015,
    hjust = 0,
    gp = gpar(
      col = "slategray",
      fontface = "italic",
      fontsize = 6
    )
  )
  
  
  # high quality
  plotQuality(score=100,
              x = 0.76 + xshift,
              y = 0.015, text=FALSE)

  grid.text(
    "High",
    x = 0.78 + xshift,
    y = 0.015,
    hjust = 0,
    gp = gpar(
      col = "slategray",
      fontface = "italic",
      fontsize = 6
    )
  )
  

  # medium quality
  plotQuality(score=50,
              x = 0.81 + xshift,
              y = 0.015, text=FALSE)
  
  grid.text(
    "Medium",
    x = 0.83 + xshift,
    y = 0.015,
    hjust = 0,
    gp = gpar(
      col = "slategray",
      fontface = "italic",
      fontsize = 6
    )
  )
  
  # lowquality
  plotQuality(score=25,
              x = 0.86 + xshift,
              y = 0.015, text=FALSE)
  
  grid.text(
    "Low",
    x = 0.88 + xshift,
    y = 0.015,
    hjust = 0,
    gp = gpar(
      col = "slategray",
      fontface = "italic",
      fontsize = 6
    )
  )
  
  
  #grid.text("Not Submitted but Worth Exploring", x=0.805+xshift, y=0.015, hjust=0, gp=gpar(col="slategray", fontface="italic", fontsize=6))
  
  grid.text(
    "Key to data item status and completeness: ",
    x = 0.365 + xshift + 0.01,
    y = 0.015,
    hjust = 1,
    gp = gpar(
      col = "steelblue4",
      fontface = "bold",
      fontsize = 8
    )
  )
  
  grid.text(
    "Key to data item quality: ",
    x = 0.75 + xshift,
    y = 0.015,
    hjust = 1,
    gp = gpar(
      col = "steelblue4",
      fontface = "bold",
      fontsize = 8
    )
  )
  
  # logos
  grid.raster(
    image = gLogo,
    x = 0.03,
    y = 0.85,
    just = "left",
    width = 0.10
  )
  grid.raster(
    image = gLogo2,
    x = 0.02,
    y = 0.04,
    just = "left",
    width = 0.10
  )
  
  
  # Summary ============================================================================
  
  # summary section of the report
  
  # Completeness, quality, and timeliness scores --------------------------------------
  tscore <- time_weight * getTimelinessScore(state, year, submission_deadline)
  cscore <-
    round(complete_weight * CompletedScore / CompletedScoreMax, 1)
  qscore <-
    round(quality_weight * QualityCross, 1)
  
  scores <- data.table(
    type = c("Timeliness", "Completeness", "Quality"),
    score = c(tscore, cscore, qscore)
  )
  
  if (tscore > 0)
  {
    grid.rect(
      x = 0.41,
      y = 0.85,
      width = unit(0.025, "npc"),
      height = unit(
        0.1 * tscore / max(time_weight, complete_weight, quality_weight),
        "npc"
      ),
      vjust = 0,
      gp = gpar(fill = "black", col = "black")
    )
  }
  if (cscore > 0)
  {
    grid.rect(
      x = 0.475,
      y = 0.85,
      width = unit(0.025, "npc"),
      height = unit(
        0.1 * cscore / max(time_weight, complete_weight, quality_weight),
        "npc"
      ),
      vjust = 0,
      gp = gpar(fill = "black", col = "black")
    )
  }
  if (qscore > 0)
  {
    grid.rect(
      x = 0.54,
      y = 0.85,
      width = unit(0.025, "npc"),
      height = unit(
        0.1 * qscore / max(time_weight, complete_weight, quality_weight),
        "npc"
      ),
      vjust = 0,
      gp = gpar(fill = "black", col = "black")
    )
  }
  
  
  grid.text(
    "Timeliness",
    x = 0.41,
    y = 0.83,
    hjust = 0.5,
    gp = gpar(fontsize = 7, col = "gray50")
  )
  grid.text(
    "Completeness",
    x = 0.475,
    y = 0.83,
    hjust = 0.5,
    gp = gpar(fontsize = 7, col = "gray50")
  )
  grid.text(
    "Quality",
    x = 0.54,
    y = 0.83,
    hjust = 0.5,
    gp = gpar(fontsize = 7, col = "gray50")
  )
  
  grid.text(
    paste0("out of ", time_weight),
    x = 0.41,
    y = 0.81,
    hjust = 0.5,
    gp = gpar(fontsize = 7, col = "gray50")
  )
  grid.text(
    paste0("out of ", complete_weight),
    x = 0.475,
    y = 0.81,
    hjust = 0.5,
    gp = gpar(fontsize = 7, col = "gray50")
  )
  grid.text(
    paste0("out of ", quality_weight),
    x = 0.54,
    y = 0.81,
    hjust = 0.5,
    gp = gpar(fontsize = 7, col = "gray50")
  )
  
  
  grid.text(
    tscore,
    x = 0.41,
    y = 0.83 + 0.1 * tscore / max(time_weight, complete_weight, quality_weight) +
      0.03,
    hjust = 0.5,
    gp = gpar(fontsize = 7, col = "black")
  )
  grid.text(
    cscore,
    x = 0.475,
    y = 0.83 + 0.1 * cscore / max(time_weight, complete_weight, quality_weight) +
      0.03,
    hjust = 0.5,
    gp = gpar(fontsize = 7, col = "black")
  )
  grid.text(
    qscore,
    x = 0.54,
    y = 0.83 + 0.1 * qscore / max(time_weight, complete_weight, quality_weight) +
      0.03,
    hjust = 0.5,
    gp = gpar(fontsize = 7, col = "black")
  )
  
  grid.text(
    "The Score is the sum of\npoints received from\ntimeliness, completeness, \nand quality.",
    y = 0.90,
    x = 0.71,
    hjust = 1,
    gp = gpar(fontsize = 10, col = "gray50")
  )
  
  grid.circle(
    x = 0.32,
    y = 0.88,
    r = unit(0.07, "npc"),
    gp = gpar(fill = "gray85", col = "gray50")
  )
  #grid.text("Overall",     x=0.31, y=0.87, just="right", gp=gpar(fontsize=18, col="slategray"))
  grid.text(
    paste0(tscore + qscore + cscore),
    x = 0.32,
    y = 0.90,
    just = "left",
    gp = gpar(fontsize = 23, col = "black"),
    hjust = 0.5
  )
  grid.text(
    paste0("out of ", time_weight + complete_weight + quality_weight),
    x = 0.32,
    y = 0.86,
    just = "left",
    gp = gpar(fontsize = 13, col = "gray50"),
    hjust = 0.5
  )
  
  return(list(quality = dt_quality, cross_validation = dt_cross))
}
