###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont
#
#
# Description:
#
# This creates one summary analysis page
#
###########################################################################

create_page_summary <- function(
  data,
  state,
  year,
  year_compare=NULL,
  x1,x2=NULL,x3=NULL, 
  color="white",
  title,icontext,
  page,
  ramps=FALSE){
  
  width <- unit(
    c(1.5, 0.05,
      4-(0.5-0.1333+0.3)/3, 0.05,
      4-(0.5-0.1333+0.3)/3, 0.05,
      4-(0.5-0.1333+0.3)/3, 0.35), units="inches")
  
  show2 <- show3 <- TRUE
  
  if(is.null(x2)){
    show2 <- FALSE
  }
  
  if(is.null(x3)){
    show3 <- FALSE
  }
  
  # Create graphics for each row of the column.
  
  # header (1, 2)
  row1 <- rectGrob(gp = gpar(fill = color, col = color)) # saves space for the header
  row2 <- rectGrob(gp = gpar(fill = "slategray", col = "white"))
  
  # titles (3)
  row3 <- arrangeGrob(
    rectGrob(gp=gpar(fill=color, col = color)),
    rectGrob(gp=gpar(fill="white", col = "white")),
    rectGrob(gp=gpar(fill=color, col = color)),
    rectGrob(gp=gpar(fill="white", col = "white")),
    if(show2){
      rectGrob(gp=gpar(fill=color, col = color))
    } else {
      rectGrob(gp=gpar(fill="white", col = "white"))
    },
    rectGrob(gp=gpar(fill="white", col = "white")),
    if(show3){
      rectGrob(gp=gpar(fill=color, col = color))
    } else {
      rectGrob(gp=gpar(fill="white", col = "white"))
    },
    rectGrob(gp = gpar(fill = "white", col = "white")),
    nrow=1, ncol=8, widths=width
  )

  # buffer (4)
  row4 <- arrangeGrob(
    rectGrob(gp=gpar(fill=color, col = color)),
    rectGrob(gp=gpar(fill="white", col = "white")),
    rectGrob(gp=gpar(fill="white", col = "white")),
    rectGrob(gp=gpar(fill="white", col = "white")),
    rectGrob(gp=gpar(fill="white", col = "white")),
    rectGrob(gp=gpar(fill="white", col = "white")),
    rectGrob(gp=gpar(fill="white", col = "white")),
    rectGrob(gp = gpar(fill = "white", col = "white")),
    nrow=1, ncol=8, widths=width
  )
  
  # Row of three tables (row 5)
  tab51 <- create_summary_report(data, state, year, gVariables[x1,Name],
                                 gVariables[x1, Data_Type], gVariables[x1,Extent],
                                 gVariables[x1,Extent_FS], ramps=ramps)
  if(show2){
    tab52 <- create_summary_report(data, state, year, gVariables[x2,Name],
                          gVariables[x2, Data_Type], gVariables[x2,Extent],
                          gVariables[x2,Extent_FS],ramps=ramps)
  } else {
    tab52 <- rectGrob(gp=gpar(fill="white", col = "white"))
  }
  
  if(show3){
    tab53 <- create_summary_report(data, state, year, gVariables[x3,Name],
                                        gVariables[x3, Data_Type], gVariables[x3,Extent],
                                        gVariables[x3,Extent_FS], ramps=ramps)
  } else {
    tab53 <- rectGrob(gp=gpar(fill="white", col = "white"))
  }
  
  row5 <- arrangeGrob(
    rectGrob(gp=gpar(fill=color, col = color)),
    rectGrob(gp=gpar(fill="white", col = "white")),
    tab51,
    rectGrob(gp=gpar(fill="white", col = "white")),
    tab52,
    rectGrob(gp=gpar(fill="white", col = "white")),
    tab53,
    rectGrob(gp = gpar(fill = "white", col = "white")),
    nrow=1,ncol=8,widths=width
  )
  
  # Row of density plots (each plot 3x2) (Row 6)
  plt61 <- suppressWarnings(
    create_travel_yoy_density(data, state, year, year_compare,
                              gVariables[x1,Name],
                              gVariables[x1,National_Data_Comparison], ramps=ramps))
  
  if(show2){
    plt62 <- suppressWarnings(
      create_travel_yoy_density(data, state, year, year_compare,
                                gVariables[x2,Name],
                                gVariables[x2,National_Data_Comparison],
                                ramps=ramps))
  } else {
    plt62 <- rectGrob(gp=gpar(fill="white", col = "white"))
  }
  
  if (show3){
    plt63 <- suppressWarnings(
      create_travel_yoy_density(data, state, year, year_compare,
                                gVariables[x3,Name], 
                                gVariables[x3,National_Data_Comparison],
                                ramps=ramps))
  } else {
    plt63 <- rectGrob(gp=gpar(fill="white", col = "white"))
  }
  
  row6 <- arrangeGrob(
    rectGrob(gp=gpar(fill=color, col = "white")),
    rectGrob(gp=gpar(fill="white", col = "white")),
    plt61,
    rectGrob(gp=gpar(fill="white", col = "white")),
    plt62,
    rectGrob(gp=gpar(fill="white", col = "white")),
    plt63,
    rectGrob(gp = gpar(fill = "white", col = "white")),
    nrow=1, ncol=8, widths=width
  )
  
  # Row of three horizontal stacked bar graphs (Row 7)
  
  plt71 <- create_travel_data_yoy(data, state, year, year_compare,
                                  gVariables[x1,Name], gVariables[x1, YOY_Hist_Type],
                                  ramps=ramps)
  
  if ( show2 ){
    plt72 <-  create_travel_data_yoy(data, state, year, year_compare,
                                     gVariables[x2,Name], gVariables[x2, YOY_Hist_Type],
                                     ramps=ramps)
  } else {
    plt72 <- rectGrob(gp=gpar(fill="white", col = "white"))
  }
  
  if ( show3 ){
    plt73 <- create_travel_data_yoy(data, state, year, year_compare,
                                    gVariables[x3,Name], gVariables[x3, YOY_Hist_Type],
                                    ramps=ramps)
  } else {
    plt73 <- rectGrob(gp=gpar(fill="white", col = "white"))
  }
  
  
  
  row7 <- arrangeGrob(
    rectGrob(gp=gpar(fill=color, col = "white")),
    rectGrob(gp=gpar(fill="white", col = "white")),
    plt71,
    rectGrob(gp=gpar(fill="white", col = "white")),
    plt72,
    rectGrob(gp=gpar(fill="white", col = "white")),
    plt73,
    rectGrob(gp = gpar(fill = "white", col = "white")),
    nrow=1, ncol=8, widths=width
  )
  
  grid.arrange(row1, row2, row3, row4, row5, row6, row7, 
               nrow=7, ncol=1,
               heights=unit(c(0.6, 0.03, 1/3+0.07, 0.083333, 1.32,
                   2.341667 + 0.65 + 0.5, 1.571667), units="inches")
    )
  
  add_header(state,year,title,icontext)
  
  add_summary_row_labels(year,year_compare,ramps=ramps)
  #browser()
  quality_results = fread(paste0("data/", gState_Labels[index==state,label],
                                 "/", gState_Labels[index==state,label],
                                 "_", year, "_", year_compare, "_quality_summary.csv"))
  
  add_summary_col_labels(
    paste0(gVariables[x1,Label],
           if(ramps){ "" } else { quality_results[Item_Number == gVariables[x1,Item_Number],
                                                  paste0("\n(o:",Outlier_Score,"/a:",Adjacency_Score,"/y:",YOY_Score,")")]}),1
  )
  
  if(show2){
    add_summary_col_labels(
    paste0(gVariables[x2,Label],
           if(ramps){""}else{quality_results[Item_Number==gVariables[x2,Item_Number],paste0("\n(o:",Outlier_Score,"/a:",Adjacency_Score,"/y:",YOY_Score,")")]}),2
    )
  }
  
  if(show3){
    add_summary_col_labels(
    paste0(gVariables[x3,Label],
           if(ramps){""}else{quality_results[Item_Number==gVariables[x3,Item_Number],paste0("\n(o:",Outlier_Score,"/a:",Adjacency_Score,"/y:",YOY_Score,")")]}),3
    )
  }
  
  add_page_number(page)
}
