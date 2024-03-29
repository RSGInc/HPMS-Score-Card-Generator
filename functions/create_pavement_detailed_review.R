###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont
#
#
# Description:
#
# Creates the second page of the pdf (after the info page)
# This is the pavement: detailed review page.
#
###########################################################################

create_pavement_detailed_review <- function(
  data,
  state,
  year,
  year_compare=NULL,
  color=gColors$blank){

  grid.arrange(
    
    # header
    rectGrob(gp = gpar(fill = color, col = color)), # saves space for the header
    rectGrob(gp = gpar(fill = gColors$dark, col = gColors$blank)), 
    
    # first row of results   
    arrangeGrob(
      rectGrob(gp = gpar(fill = gColors$blank, col=gColors$blank)),
      rectGrob(gp = gpar(fill = gColors$blank, col=gColors$blank)),
      
      #create_overall_condition(data,state,year,population),
      create_pavement_summary(data, state, year),
      
      rectGrob(gp = gpar(fill = gColors$blank, col = gColors$blank)),
      nrow = 1,widths=unit(c(0.167,2,4.5,0.33+5.836+0.5),units="inches")
    ),
    rectGrob(gp = gpar(fill = "gray70", col = gColors$blank)), 
    rectGrob(gp = gpar(fill = gColors$blank, col = gColors$blank)), 
    
    
    # titles for the next set of results
    
    arrangeGrob(
      rectGrob(gp = gpar(fill = gColors$blank, col = gColors$blank)),
      textGrob("",just="right",gp=gpar(fontsize=12, col=gColors$text)),
      textGrob("51 - Faulting (SP)",just="centre",gp=gpar(fontsize=10, col=gColors$dark,fontface="bold")),
      textGrob("52 - Cracking Percent (SP)",just="centre",gp=gpar(fontsize=10, col=gColors$dark,fontface="bold")),
      textGrob("50 - Rutting (SP)",just="centre",gp=gpar(fontsize=10, col=gColors$dark,fontface="bold")),
      textGrob("47 - International Roughness Index (FE*)",just="centre",gp=gpar(fontsize=10, col=gColors$dark,fontface="bold")),
      rectGrob(gp = gpar(fill = gColors$blank, col = gColors$blank)),
      nrow=1,widths=unit(c(0.167,1.5,rep(11.336/4,4),0.33),units="inches")
    ),
    
    # first row of tables
    arrangeGrob(
      rectGrob(gp = gpar(fill = gColors$blank, col = gColors$blank)),
      rectGrob(gp = gpar(fill = gColors$blank, col = gColors$blank)),
      create_table(
        create_outlier_report(data,year,"FAULTING")),
      create_table(
        create_outlier_report(data,year,"CRACKING_PERCENT")),
      create_table(
        create_outlier_report(data,year,"RUTTING")),
      create_table(
        create_outlier_report(data,year,"IRI")),
      rectGrob(gp = gpar(fill = gColors$blank, col = gColors$blank)),
      nrow = 1,widths=unit(c(0.167,1.5,rep(11.336/4,4),0.33),units="inches")),
    
    # second row of tables
    arrangeGrob(
      rectGrob(gp = gpar(fill = gColors$blank, col = gColors$blank)),
      rectGrob(gp = gpar(fill = gColors$blank, col = gColors$blank)),
      create_table(
        create_adjacency_report(data,year,"FAULTING")),
      create_table(
        create_adjacency_report(data,year,"CRACKING_PERCENT")),
      create_table(
        create_adjacency_report(data,year,"RUTTING")),
      create_table(
        create_adjacency_report(data,year,"IRI")),
      rectGrob(gp = gpar(fill = gColors$blank, col = gColors$blank)),
      nrow = 1,widths=unit(c(0.167,1.5,rep(11.336/4,4),0.33),units="inches")),
    
    # third row of results   
    arrangeGrob(
      rectGrob(gp = gpar(fill = gColors$blank, col = gColors$blank)),
      rectGrob(gp = gpar(fill = gColors$blank, col = gColors$blank)),
      create_table(
        create_yearoveryear_report(data,year,"FAULTING",year_compare)),
      create_table(
        create_yearoveryear_report(data,year,"CRACKING_PERCENT",year_compare)),
      create_table(
        create_yearoveryear_report(data,year,"RUTTING",year_compare)),
      create_table(
        create_yearoveryear_report(data,year,"IRI",year_compare)),
      rectGrob(gp = gpar(fill = gColors$blank, col = gColors$blank)),
      nrow = 1,widths=unit(c(0.167,1.5,rep(11.336/4,4),0.33),units="inches")),
    nrow=9,heights = unit(c(0.6,0.03,1.25,0.03,0.1,0.25,rep(5.24/3,3)),units="inches"))
  
  add_header(state,year,"pavement: detailed review","p")
  
  #grid.text("Pavement Summary",x=0.12,y=0.87,hjust=1,gp=gpar(fontsize=10, col=gColors$dark))
  grid.text("Analysis of Through Lanes\nand Surface Type",x=0.16,y=0.84,hjust=1,gp=gpar(fontsize=10, col=gColors$dark))
  grid.text("(Counts of sections)",x=0.16,y=0.8,hjust=1,gp=gpar(fontsize=7, col=gColors$dark))
  
  grid.text("Outliers",hjust=1,x=0.12,y=0.67,gp=gpar(fontsize=10, col=gColors$dark))
  grid.text("Sections\nwith outlying values",x=0.12,y=0.63,hjust=1,gp=gpar(fontsize=7, fontface="italic",col=gColors$dark))
  
  grid.text("Adjacent Sections",x=0.12,y=0.445,hjust=1,gp=gpar(fontsize=10, col=gColors$dark))
  grid.text(paste0("Adjacent sections\nwith the same value\nwithin ",year),x=0.12,y=0.405,hjust=1,gp=gpar(fontsize=7, fontface="italic",col=gColors$dark))
  
  grid.text(paste0(year_compare,"-",year," Sections"),x=0.12,y=0.22,hjust=1,gp=gpar(fontsize=10, col=gColors$dark))
  grid.text("Sections\nwith the same value\nas previous year",x=0.12,y=0.18,hjust=1,gp=gpar(fontsize=7, fontface="italic",col=gColors$dark))
  
  grid.text(paste0("Outliers: <", gVariables[Name=="FAULTING",Outlier_Min],        " | >",gVariables[Name=="FAULTING",Outlier_Max],"\"",        " (% > ",gVariables[Name=="FAULTING",OH_Thresh]," are highlighted)"),        x=0.16,y=0.56,hjust=0,gp=gpar(fontsize=5.75, fontface="italic",col=gColors$dark))
  grid.text(paste0("Outliers: <", gVariables[Name=="CRACKING_PERCENT",Outlier_Min]," | >",gVariables[Name=="CRACKING_PERCENT",Outlier_Max],"%", " (% > ",gVariables[Name=="CRACKING_PERCENT",OH_Thresh]," are highlighted)"),x=0.37,y=0.56,hjust=0,gp=gpar(fontsize=5.75, fontface="italic",col=gColors$dark))
  grid.text(paste0("Outliers: <", gVariables[Name=="RUTTING",Outlier_Min],         " | >",gVariables[Name=="RUTTING",Outlier_Max],"\"",         " (% > ",gVariables[Name=="RUTTING",OH_Thresh]," are highlighted)"),         x=0.585,y=0.56,hjust=0,gp=gpar(fontsize=5.75, fontface="italic",col=gColors$dark))
  grid.text(paste0("Outliers: <", gVariables[Name=="IRI",Outlier_Min],             " | >",gVariables[Name=="IRI",Outlier_Max],"",               " (% > ",gVariables[Name=="IRI",OH_Thresh]," are highlighted)"),             x=0.7975,y=0.56,hjust=0,gp=gpar(fontsize=5.75, fontface="italic",col=gColors$dark))
  
  grid.text(paste0("% > ", gVariables[Name=="FAULTING",AH_Thresh]," are highlighted"),        x=0.16,y=0.33,hjust=0,gp=gpar(fontsize=5.75, fontface="italic",col=gColors$dark))
  grid.text(paste0("% > ", gVariables[Name=="CRACKING_PERCENT",AH_Thresh]," are highlighted"),x=0.37,y=0.33,hjust=0,gp=gpar(fontsize=5.75, fontface="italic",col=gColors$dark))
  grid.text(paste0("% > ", gVariables[Name=="RUTTING",AH_Thresh]," are highlighted"),         x=0.585,y=0.33,hjust=0,gp=gpar(fontsize=5.75, fontface="italic",col=gColors$dark))
  grid.text(paste0("% > ", gVariables[Name=="IRI",AH_Thresh]," are highlighted"),             x=0.7975,y=0.33,hjust=0,gp=gpar(fontsize=5.75, fontface="italic",col=gColors$dark))
  
  grid.text(paste0("% > ", gVariables[Name=="FAULTING",YOYH_Thresh]," are highlighted"),        x=0.16,y=0.1,hjust=0,gp=gpar(fontsize=5.75, fontface="italic",col=gColors$dark))
  grid.text(paste0("% > ", gVariables[Name=="CRACKING_PERCENT",YOYH_Thresh]," are highlighted"),x=0.37,y=0.1,hjust=0,gp=gpar(fontsize=5.75, fontface="italic",col=gColors$dark))
  grid.text(paste0("% > ", gVariables[Name=="RUTTING",YOYH_Thresh]," are highlighted"),         x=0.585,y=0.1,hjust=0,gp=gpar(fontsize=5.75, fontface="italic",col=gColors$dark))
  grid.text(paste0("% > ", gVariables[Name=="IRI",YOYH_Thresh]," are highlighted"),             x=0.7975,y=0.1,hjust=0,gp=gpar(fontsize=5.75, fontface="italic",col=gColors$dark))
  
  gPageNumber <<- gPageNumber + 1
  add_page_number(gPageNumber)
  
  
  
}