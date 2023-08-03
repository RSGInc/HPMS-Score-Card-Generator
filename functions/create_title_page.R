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
  
  # if ( debugmode ) browser()
  
  # Load data -----------------------------------------------------------------
  
  state_num = data$state_code[1]
  state_name = getStateLabelFromNum(state_num)
  state_abb = getStateAbbrFromNum(state_num)
    
  time_weight     <- gScoreWeights[1, timeliness]
  complete_weight <- gScoreWeights[1, completeness]
  quality_weight  <- gScoreWeights[1, quality]
  cvWeight        <- gScoreWeights[1, cross_validation]
  
  reqs = gReqs[, c('Name', state_abb), with=FALSE]
  setnames(reqs, state_abb, 'required')
  
  data_summary <- create_data_summary(data, state, year, year_compare)
  data_summary <- data_summary[nrow(data_summary):1,]
  #data_summary <- data.frame(data_summary)

  message("Calculating coverage validation results. This may take some time to complete.")
  dt_coverage <- calc_completeness_all(data, year, reqs)
  
  message("Calculating quality score for each item.")
  dt_quality <- calc_quality_all(data, year, year_compare)
  
  message("Calculating cross-validations.")
  dt_cross <- calc_cross_validation(data, year)
  
  
  CompletedScore <- 0
  CompletedScoreMax <- 0
  
  #QualityScore <- 0
  submittedN   <- 0
  #QualityScoreMax <- 0
  
  # Scores for getting a medium or high quality value
  CompleteLow = 0
  CompleteMed = 1
  CompleteHigh = 1.5
  
  dt_coverage[coverage_type == 1, card_score := CompleteLow]
  dt_coverage[coverage_type == 2, card_score := CompleteMed]
  dt_coverage[coverage_type == 3, card_score := CompleteHigh]
  
  # Take into account Curves and grades (only one of A-F required)
  dt_other = dt_coverage[!Name %like% 'CURVES|GRADES']
  dt_curves = dt_coverage[Name %like% 'CURVES'][which.max(coverage_score)]
  dt_grades = dt_coverage[Name %like% 'GRADES'][which.max(coverage_score)]
  
  
  dt_coverage2 = merge(
    rbind(dt_other, dt_curves, dt_grades),
    dt_quality[, .(Name, Completeness_Weight)],
    by = 'Name',
    all.x=TRUE
  )
  
  # Account for collapse of CURVES and GRADES into a single row
  stopifnot((dt_coverage[, .N] - dt_coverage2[, .N]) == 2 * 5)
  
  submittedN = dt_coverage2[coverage_type >= 2, .N]
  CompletedScore = dt_coverage2[, sum(card_score * Completeness_Weight, na.rm=TRUE)]
  CompletedScoreMax = dt_coverage2[, sum(CompleteHigh * (!is.na(card_score)) * Completeness_Weight)]
  
  # FIXME: items not required should not count towards top level score
  dt_quality[reqs, required := i.required, on = 'Name']
  QualityScore    <- dt_quality[, sum(Quality_Score * Quality_Weight, na.rm=TRUE)]
  QualityScoreMax <- dt_quality[, sum(!is.na(Quality_Score) * Quality_Weight) * 100]
  qMean <- QualityScore / QualityScoreMax
  
  # Incorporate cross-validation score
  cvMean <- mean(dt_cross$mileage_pass, na.rm=TRUE)
  
  QualityCross <- qMean * (1 - cvWeight) + cvMean * cvWeight  
  
  #(cvMean * nrow(dt_cross) + qMean * nrow(dt_quality)) /
  #  (nrow(dt_cross) + nrow(dt_quality))
  
  
  # Summary scores ------------------------------------------------------------
  # Completeness, quality, and timeliness scores 
  
  tscore <- time_weight * getTimelinessScore(state, year, submission_deadline)
  cscore <-
    round(complete_weight * CompletedScore / CompletedScoreMax, 1)
  qscore <-
    round(quality_weight * QualityCross, 1)
  
  
  # Write scores ---------------------------------------------------
  
  # These are read in and used for individual pages, e.g. in create_page_summary
  dt_scores = merge(
    dt_quality,
    dt_coverage,
    by = 'Name',
    all = TRUE
  )
  
  path <- file.path('data', state_name)
  file <- paste0(state_name, '_', year, '_', year_compare, '_item_score_summary.csv')
  fullpath <- file.path(path, file)
  
  if (!dir.exists(path)) dir.create(path)
  
  fwrite(x=dt_scores, file=fullpath, na = '')
  
  # Write out the cross-validation scores
  
  path <- file.path('data', state_name)
  file <- paste0(state_name, '_', year,
                 '_cross_validation_summary.csv')
  fullpath <- file.path(path, file)
  
  if (!dir.exists(path)) dir.create(path)
  
  fwrite(x=dt_cross, file=fullpath, na='')
  
  
  # Write out high level scores
  
  scores <- data.table(
    state_num = state_num,
    state = state_abb,
    timely = tscore,
    complete = cscore,
    quality = qscore,
    quality_sub = qMean,
    cross_sub = cvMean,
    total = tscore + qscore + cscore,
    datetime = now()
  )
  
  fullpath = file.path('output/_score_summary.csv')
  
  if ( file.exists(fullpath) ){
    all_scores = fread(file=fullpath)
    all_scores = all_scores[state != state_abb]
    all_scores = rbind(all_scores, scores)
    fwrite(all_scores, fullpath)
    
  } else {
    
    fwrite(scores, fullpath)
    
  }
  
  
  # Draw page ==============================================================
  
  
  # Graphics parameters ----------------------------------------------------
  
  col_header = gColors$accent 
  col_item_label = gColors$dark  
  col_text = gColors$text
  col_blank = gColors$blank
  col_background = gColors$text_background
  
  hdr_left <- 0.25  # Where do the headers start?
  hdr_top <- 0.98 
  
  sec_header_gp <-
    gpar(col = col_header,
         fontface = "bold",
         fontsize = 13)
  line_gp <- gpar(col = col_item_label, lty = 3)
  
  
  # Page setup -------------------------------------------------------------
  
  grid.arrange(
    arrangeGrob(
      rectGrob(gp = gpar(fill = col_background, col = col_background)),
      rectGrob(gp = gpar(fill = col_blank, col = col_blank)),
      rectGrob(gp = gpar(fill = "gray20", col = "gray20")),
      
      arrangeGrob(
        rectGrob(gp = gpar(fill = col_background, col = col_background)),
        rectGrob(gp = gpar(fill = col_background, col = col_background)),
        rectGrob(gp = gpar(fill = col_background, col = col_background)),
        rectGrob(gp = gpar(fill = col_background, col = col_background)),
        rectGrob(gp = gpar(fill = col_background, col = col_background)),
        rectGrob(gp = gpar(fill = col_background, col = col_background)),
        rectGrob(gp = gpar(fill = col_blank, col = col_blank)),
        rectGrob(gp = gpar(fill = col_blank, col = col_blank)),
        rectGrob(gp = gpar(fill = col_blank, col = col_blank)),
        rectGrob(gp = gpar(fill = col_blank, col = col_blank)),
        rectGrob(gp = gpar(fill = col_blank, col = col_blank)),
        rectGrob(gp = gpar(fill = col_blank, col = col_blank)),
        rectGrob(gp = gpar(fill = col_blank, col = col_blank)),
        rectGrob(gp = gpar(fill = col_blank, col = col_blank)),
        rectGrob(gp = gpar(fill = col_blank, col = col_blank)),
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
  
  
  # Sidebar -----------------------------------------------------------------
  
  grid.text(
    "HPMS SCORECARD",
    x = 0.03,
    y = 0.68,
    just = "left",
    gp = gpar(
      col = col_item_label,
      fontface = "bold",
      fontsize = 10
    )
  )
  
  grid.draw(linesGrob(
    x = unit(c(0.03, 0.18), "npc"),
    y = unit(c(0.66, 0.66), "npc"),
    gp = gpar(col = col_text, lty = 1)
  ))
  
  grid.text(
    toupper(gState_Labels[index == state, label]),
    x = 0.03,
    y = 0.63,
    just = "left",
    gp = gpar(
      col = col_text,
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
      col = col_text,
      fontface = "bold",
      fontsize = 27
    )
  )
  
  grid.draw(linesGrob(
    x = unit(c(0.03, 0.18), "npc"),
    y = unit(c(0.505, 0.505), "npc"),
    gp = gpar(col = col_text, lty = 1)
  ))
  
  grid.text(
    paste0("Generated: ", format(Sys.time(), "%B %d, %Y")),
    x = 0.03,
    y = 0.52,
    just = "left",
    gp = gpar(
      col = col_text,
      fontface = "italic",
      fontsize = 7
    )
  )
  
  grid.text(
    title_text,
    x = 0.03,
    y = 0.285,
    just = "left",
    gp = gpar(col = col_text, fontsize = 6)
  )
  
  # logos

  grid.raster(
    image = gLogo2,
    x = 0.02,
    y = 0.04,
    just = "left",
    width = 0.10
  )
  
  grid.raster(
    image = gLogo,
    x = 0.03,
    y = 0.85,
    just = "left",
    width = 0.10
  )
  
  
  # High level scores --------------------------------------------------------
  
  grid.text("Score"           ,
            hdr_left,
            hdr_top,
            hjust = 0,
            gp = sec_header_gp)
  
  if (tscore > 0){
    grid.rect(
      x = 0.41,
      y = 0.85,
      width = unit(0.025, "npc"),
      height = unit(
        0.1 * tscore / max(time_weight, complete_weight, quality_weight),
        "npc"
      ),
      vjust = 0,
      gp = gpar(fill = col_text, col = col_text)
    )
  }
  
  if (cscore > 0){
    grid.rect(
      x = 0.475,
      y = 0.85,
      width = unit(0.025, "npc"),
      height = unit(
        0.1 * cscore / max(time_weight, complete_weight, quality_weight),
        "npc"
      ),
      vjust = 0,
      gp = gpar(fill = col_text, col = col_text)
    )
  }
  
  if (qscore > 0){
    grid.rect(
      x = 0.54,
      y = 0.85,
      width = unit(0.025, "npc"),
      height = unit(
        0.1 * qscore / max(time_weight, complete_weight, quality_weight),
        "npc"
      ),
      vjust = 0,
      gp = gpar(fill = col_text, col = col_text)
    )
  }
  
  
  grid.text(
    "Timeliness",
    x = 0.41,
    y = 0.83,
    hjust = 0.5,
    gp = gpar(fontsize = 7, col = col_item_label)
  )
  grid.text(
    "Completeness",
    x = 0.475,
    y = 0.83,
    hjust = 0.5,
    gp = gpar(fontsize = 7, col = col_item_label)
  )
  grid.text(
    "Quality",
    x = 0.54,
    y = 0.83,
    hjust = 0.5,
    gp = gpar(fontsize = 7, col = col_item_label)
  )
  
  grid.text(
    paste0("out of ", time_weight),
    x = 0.41,
    y = 0.81,
    hjust = 0.5,
    gp = gpar(fontsize = 7, col = col_item_label)
  )
  grid.text(
    paste0("out of ", complete_weight),
    x = 0.475,
    y = 0.81,
    hjust = 0.5,
    gp = gpar(fontsize = 7, col = col_item_label)
  )
  grid.text(
    paste0("out of ", quality_weight),
    x = 0.54,
    y = 0.81,
    hjust = 0.5,
    gp = gpar(fontsize = 7, col = col_item_label)
  )
  
  
  grid.text(
    tscore,
    x = 0.41,
    y = 0.83 + 0.1 * tscore / max(time_weight, complete_weight, quality_weight) +
      0.03,
    hjust = 0.5,
    gp = gpar(fontsize = 7, col = col_text)
  )
  grid.text(
    cscore,
    x = 0.475,
    y = 0.83 + 0.1 * cscore / max(time_weight, complete_weight, quality_weight) +
      0.03,
    hjust = 0.5,
    gp = gpar(fontsize = 7, col = col_text)
  )
  grid.text(
    qscore,
    x = 0.54,
    y = 0.83 + 0.1 * qscore / max(time_weight, complete_weight, quality_weight) +
      0.03,
    hjust = 0.5,
    gp = gpar(fontsize = 7, col = col_text)
  )
  
  grid.text(
    "The Score is the sum of\npoints received from\ntimeliness, completeness, \nand quality.",
    y = 0.90,
    x = 0.71,
    hjust = 1,
    gp = gpar(fontsize = 10, col = col_item_label)
  )
  
  grid.circle(
    x = 0.32,
    y = 0.88,
    r = unit(0.07, "npc"),
    gp = gpar(fill = "gray85", col = col_item_label)
  )
  #grid.text("Overall",     x=0.31, y=0.87, just="right", gp=gpar(fontsize=18, col=col_item_label))
  grid.text(
    paste0(tscore + qscore + cscore),
    x = 0.32,
    y = 0.90,
    just = "left",
    gp = gpar(fontsize = 23, col = col_text),
    hjust = 0.5
  )
  grid.text(
    paste0("out of ", time_weight + complete_weight + quality_weight),
    x = 0.32,
    y = 0.86,
    just = "left",
    gp = gpar(fontsize = 13, col = col_item_label),
    hjust = 0.5
  )
  
  
  # Data summary ==============================================================
  
  
  grid.draw(linesGrob(
    x = unit(c(0.73, 0.73), "npc"),
    y = unit(c(1, 0.79), "npc"),
    gp = gpar(col = col_blank, lty = 1, lwd = 3)
  ))
  
  label_gp <- gpar(fontsize = 8,
                   fontface = "bold",
                   col = col_item_label)
  item_gp <- gpar(fontsize = 8, col = col_text)
  
  y_bot <- 0.825
  
  x_label <- 0.85
  x_col1 <- x_label + 0.055
  x_col2 <- x_col1 + 0.055
  y_row_spc <- 0.02
  
  grid.text("Data Summary"    ,
            x_label,
            hdr_top,
            hjust = 1,
            gp = sec_header_gp)
  
  grid.text(
    paste0("", year),
    x = x_col1,
    y = y_bot + y_row_spc * (nrow(data_summary) - 1) + 0.005,
    gp = gpar(
      fontsize = 9,
      fontface = "bold",
      col = col_item_label
    ),
    hjust = 1
  )
  
  grid.text(
    paste0("", year_compare),
    x = x_col2,
    y = y_bot + y_row_spc * (nrow(data_summary) - 1) + 0.005,
    gp = gpar(
      fontsize = 9,
      fontface = "bold",
      col = col_item_label
    ),
    hjust = 1
  )
  
  # Draw labels
  for (i in 1:(nrow(data_summary) - 1)) {
    grid.text(
      data_summary[i, 1, with = FALSE],
      x = x_label,
      y = y_bot + y_row_spc * (i - 1),
      gp = label_gp,
      hjust = 1
    )
    grid.text(
      data_summary[i, 2, with = FALSE],
      x = x_col1,
      y = y_bot + y_row_spc * (i - 1),
      gp = item_gp,
      hjust = 1
    )
    grid.text(
      data_summary[i, 3, with = FALSE],
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
      col = col_item_label
    ),
    hjust = 0.5
  )
  
  
  # Completeness and quality ================================================
  # The following section calculates completeness and quality for each data item
  # Then plots it.
  
  vertical_adj <- 0.05
  
  # inventory
  grid.text("inventory"       ,
            hdr_left,
            0.823 - vertical_adj,
            hjust = 0,
            gp = sec_header_gp)

  grid.draw(linesGrob(
    x = unit(c(hdr_left, 0.97), "npc"),
    y = unit(c(0.8045, 0.8045) - vertical_adj, "npc"),
    gp = line_gp
  ))

  # pavement
  grid.text("pavement"        ,
            hdr_left,
            0.665 - vertical_adj,
            hjust = 0,
            gp = sec_header_gp)

  grid.draw(linesGrob(
    x = unit(c(hdr_left, 0.97), "npc"),
    y = unit(c(0.6465, 0.6465) - vertical_adj, "npc"),
    gp = line_gp
  ))

  # traffic
  grid.text("traffic"         ,
            hdr_left,
            0.535 - vertical_adj,
            hjust = 0,
            gp = sec_header_gp)

  grid.draw(linesGrob(
    x = unit(c(hdr_left, 0.97), "npc"),
    y = unit(c(0.5165, 0.5165) - vertical_adj, "npc"),
    gp = line_gp
  ))
  
  # geometric
  grid.text("geometric"       ,
            hdr_left,
            0.387 - vertical_adj,
            hjust = 0,
            gp = sec_header_gp)
  
  grid.draw(linesGrob(
    x = unit(c(hdr_left, 0.97), "npc"),
    y = unit(c(0.3685, 0.3685) - vertical_adj, "npc"),
    gp = line_gp
  ))

  # route
  grid.text("route"           ,
            hdr_left,
            0.237 - vertical_adj,
            hjust = 0,
            gp = sec_header_gp)
  
  grid.draw(linesGrob(
    x = unit(c(hdr_left, 0.97), "npc"),
    y = unit(c(0.2185, 0.2185) - vertical_adj, "npc"),
    gp = line_gp
  ))
  
  
  # special network
  
  grid.text("special networks",
            hdr_left,
            0.14 - vertical_adj,
            hjust = 0,
            gp = sec_header_gp)
  
  
  grid.draw(linesGrob(
    x = unit(c(hdr_left, 0.97), "npc"),
    y = unit(c(0.12, 0.12) - vertical_adj, "npc"),
    gp = line_gp
  ))
  
  
  colWidth <- 0.152
  rowWidth <- 0.020 
  space1 <- 0.008    # Space between start of text and completeness symbol
  space2 <- 0.008   # Space between completeness symbol and quality symbol
  
  
  # Set parameters for each section
  
  group_params <- data.frame(
    Grouping = c('Inventory', 'Pavement', 'Traffic', 'Geometric', 'Route', 'Special Networks'),
    abbrev   = c( 'I', 'P', 'T', 'G', 'R', 'SN'),
    starty   = c(0.79, 0.63, 0.5, 0.35, 0.2, 0.105),
    nRow     = c(   4,    3,   3,    5,   1,     1))
  
  # completeness / coverage validation ----------------------------------------
  
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
        gp = gpar(col = col_item_label, fontsize = 7)
      ))
      
      plotCompleteness(
        x = startx + (C - 1) * colWidth + space1,
        y = starty - (R - 1) * rowWidth,
        score = dt_coverage[Name == variable, coverage_type],
        item_required = dt_coverage[Name == variable, required]
      )
      
      plotQuality(group_vars$Quality_Score[i],
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
  
  
  
  # legend ----------------------------------------------------------------
  
  
  grid.draw(linesGrob(
    x = unit(c(hdr_left, 0.97), "npc"),
    y = unit(c(0.03, 0.03), "npc"),
    gp = gpar(col = "gray60")
  ))
  
  y <- 0.015
  
  # Completeness ----
  
  grid.text(
    "Key to data item completeness: ",
    x = hdr_left,
    y = y,
    hjust = 0,
    gp = gpar(
      col = col_header,
      fontface = "bold",
      fontsize = 8
    )
  )

  xshift <- hdr_left + 0.15
  
  plotCompleteness(
    x = xshift,
    y = y,
    score = 3)
  
  grid.text(
    "Complete",
    x = xshift + space1,
    y = y,
    hjust = 0,
    gp = gpar(
      col = col_item_label,
      fontface = "italic",
      fontsize = 6
    )
  )
  
  
  plotCompleteness(
    x = xshift + 0.045,
    y = y,
    score = 2)
  
  grid.text(
    "Incomplete",
    x = xshift + 0.045 + space1,
    y = y,
    hjust = 0,
    gp = gpar(
      col = col_item_label,
      fontface = "italic",
      fontsize = 6
    )
  )
  
  plotCompleteness(
    x = xshift + 0.09,
    y = y,
    score = 1)
  
  grid.text(
    "Not Submitted",
    x = xshift + 0.09 + space1,
    y = y,
    hjust = 0,
    gp = gpar(
      col = col_item_label,
      fontface = "italic",
      fontsize = 6
    )
  )
  
  plotCompleteness(
    x = xshift + 0.145,
    y = y,
    score = NA
  )
  
  grid.text(
    "Not Required",
    x = xshift + 0.145 + space1,
    y=y,
    hjust = 0,
    gp = gpar(
      col= col_item_label,
      fontface='italic',
      fontsize=6
    )
  )
  
  # quality ------
  
  
  xshift <- hdr_left + 0.15 + 0.2
  
  grid.text(
    "Key to data item quality: ",
    x = xshift,
    y = y,
    hjust = 0,
    gp = gpar(
      col = col_header,
      fontface = "bold",
      fontsize = 8
    )
  )

  plotQuality(
    score=100,
    x = xshift + 0.11,
    y = y,
    text=FALSE)
  
  # high quality
  grid.text(
    "High",
    x = xshift + 0.11 + 2.5*space1,
    y = y,
    hjust = 0,
    gp = gpar(
      col = col_item_label,
      fontface = "italic",
      fontsize = 6
    )
  )
  

  # medium quality
  
  plotQuality(
    score=50,
    x = xshift + 0.15,
    y = y, text=FALSE)
  
  grid.text(
    "Medium",
    x = xshift + 0.15 + 2.5*space1,
    y = y,
    hjust = 0,
    gp = gpar(
      col = col_item_label,
      fontface = "italic",
      fontsize = 6
    )
  )
  
  
  # lowquality
  plotQuality(
    score=25,
    x = xshift + 0.195,
    y = y, text=FALSE)
  
  grid.text(
    "Low",
    x = xshift + 0.195 + 2.5*space1,
    y = y,
    hjust = 0,
    gp = gpar(
      col = col_item_label,
      fontface = "italic",
      fontsize = 6
    )
  )
  
  # No data
  plotQuality(
    score=NA,
    x = xshift + 0.24,
    y = y, text=FALSE)
  
  grid.text(
    "No Data",
    x = xshift + 0.24 + 2.5*space1,
    y = y,
    hjust = 0,
    gp = gpar(
      col = col_item_label,
      fontface = "italic",
      fontsize = 6
    )
  )
  
  #grid.text("Not Submitted but Worth Exploring", x=0.805+xshift, y=y, hjust=0, gp=gpar(col=col_item_label, fontface="italic", fontsize=6))
  
  
  
  
  return(list(quality = dt_quality, cross_validation = dt_cross, coverage=dt_coverage))
}
