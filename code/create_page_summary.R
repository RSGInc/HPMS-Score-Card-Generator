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

create_page_summary <- function(
     data,
     state,
     year,
     year_compare=NULL,
     x1,x2=NULL,x3=NULL, 
     color="white",
     title,icontext,
     page,
     ramps=FALSE)
{
     
     width <- unit(c(1.5,0.05,4-(0.5-0.1333+0.3)/3,0.05,4-(0.5-0.1333+0.3)/3,0.05,4-(0.5-0.1333+0.3)/3,0.35),units="inches")
     
     show2 <- show3 <- TRUE
     
     if(is.null(x2))
     {
          show2 <- FALSE
     }
     
     if(is.null(x3))
     {
          show3 <- FALSE
     }
     
     grid.arrange(
          # header
          rectGrob(gp = gpar(fill = color, col = color)), # saves space for the header
          
          rectGrob(gp = gpar(fill = "slategray", col = "white")), 
          # titles
          arrangeGrob(
               rectGrob(gp=gpar(fill=color, col = color)),
               rectGrob(gp=gpar(fill="white", col = "white")),
               rectGrob(gp=gpar(fill=color, col = color)),
               rectGrob(gp=gpar(fill="white", col = "white")),
               if(show2){
                    rectGrob(gp=gpar(fill=color, col = color))
               } else
               {
                    rectGrob(gp=gpar(fill="white", col = "white"))
               },
               rectGrob(gp=gpar(fill="white", col = "white")),
               if(show3){
                    rectGrob(gp=gpar(fill=color, col = color))
               } else
               {
                    rectGrob(gp=gpar(fill="white", col = "white"))
               },
               rectGrob(gp = gpar(fill = "white", col = "white")),
               nrow=1,ncol=8,widths=width
          ),
          # buffer
          arrangeGrob(
               rectGrob(gp=gpar(fill=color, col = color)),
               rectGrob(gp=gpar(fill="white", col = "white")),
               rectGrob(gp=gpar(fill="white", col = "white")),
               rectGrob(gp=gpar(fill="white", col = "white")),
               rectGrob(gp=gpar(fill="white", col = "white")),
               rectGrob(gp=gpar(fill="white", col = "white")),
               rectGrob(gp=gpar(fill="white", col = "white")),
               rectGrob(gp = gpar(fill = "white", col = "white")),
               nrow=1,ncol=8,widths=width
          ), 
          # first row
          arrangeGrob(
               rectGrob(gp=gpar(fill=color, col = color)),
               rectGrob(gp=gpar(fill="white", col = "white")),
               create_summary_report(data,state,year,gVariables[x1,Name],gVariables[x1,Type],gVariables[x1,Extent],gVariables[x1,Extent_FS],ramps=ramps),
               rectGrob(gp=gpar(fill="white", col = "white")),
               if(show2){
                    create_summary_report(data,state,year,gVariables[x2,Name],gVariables[x2,Type],gVariables[x2,Extent],gVariables[x2,Extent_FS],ramps=ramps)
               } else
               {
                    rectGrob(gp=gpar(fill="white", col = "white"))
               },
               rectGrob(gp=gpar(fill="white", col = "white")),
               if(show3)
               {
                    create_summary_report(data,state,year,gVariables[x3,Name],gVariables[x3,Type],gVariables[x3,Extent],gVariables[x3,Extent_FS],ramps=ramps)
               } else
               {
                    rectGrob(gp=gpar(fill="white", col = "white"))    
               },
               rectGrob(gp = gpar(fill = "white", col = "white")),
               nrow=1,ncol=8,widths=width
          ),
          # second row
          arrangeGrob(
               rectGrob(gp=gpar(fill=color, col = "white")),
               rectGrob(gp=gpar(fill="white", col = "white")),
               create_travel_yoy_density(data,state,year,year_compare,gVariables[x1,Name],gVariables[x1,National_Data_Comparison],ramps=ramps),
               rectGrob(gp=gpar(fill="white", col = "white")),
               if(show2)
               {
                    create_travel_yoy_density(data,state,year,year_compare,gVariables[x2,Name],gVariables[x2,National_Data_Comparison],ramps=ramps)
               } else 
               {
                    rectGrob(gp=gpar(fill="white", col = "white"))
               },
               rectGrob(gp=gpar(fill="white", col = "white")),
               if(show3)
               {
                    create_travel_yoy_density(data,state,year,year_compare,gVariables[x3,Name],gVariables[x3,National_Data_Comparison],ramps=ramps)
               } else 
               {
                    rectGrob(gp=gpar(fill="white", col = "white"))
               },
               rectGrob(gp = gpar(fill = "white", col = "white")),
               nrow=1,ncol=8,widths=width
          ),
          # third row
          arrangeGrob(
               rectGrob(gp=gpar(fill=color, col = "white")),
               rectGrob(gp=gpar(fill="white", col = "white")),
               create_travel_data_yoy(data,state,year,year_compare,gVariables[x1,Name],gVariables[x1,HistType],ramps=ramps),
               rectGrob(gp=gpar(fill="white", col = "white")),
               if(show2)
               {
                    create_travel_data_yoy(data,state,year,year_compare,gVariables[x2,Name],gVariables[x2,HistType],ramps=ramps)
               } else 
               {
                    rectGrob(gp=gpar(fill="white", col = "white"))
               },
               rectGrob(gp=gpar(fill="white", col = "white")),
               if(show3)
               {
                    create_travel_data_yoy(data,state,year,year_compare,gVariables[x3,Name],gVariables[x3,HistType],ramps=ramps)
               } else 
               {
                    rectGrob(gp=gpar(fill="white", col = "white"))
               },
               rectGrob(gp = gpar(fill = "white", col = "white")),
               nrow=1,ncol=8,widths=width
          ),
          nrow=7,ncol=1,heights=unit(c(0.6,0.03,1/3+0.07,0.083333,1.32,2.341667+0.65+0.5,1.571667),units="inches"))
     
     add_header(state,year,title,icontext)
     
     add_summary_row_labels(year,year_compare)
     
          add_summary_col_labels(paste0(gVariables[x1,Item_Number]," - ",gVariables[x1,Label]," (",gVariables[x1,Extent],")"),1)
     if(show2){
          add_summary_col_labels(paste0(gVariables[x2,Item_Number]," - ",gVariables[x2,Label]," (",gVariables[x2,Extent],")"),2)
     }
     if(show3){
          add_summary_col_labels(paste0(gVariables[x3,Item_Number]," - ",gVariables[x3,Label]," (",gVariables[x3,Extent],")"),3)
     }
     
     add_page_number(page)
}
