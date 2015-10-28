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

create_page2 <- function(
     data,
     state,
     year,
     year_compare=NULL,
     color="white",
     population)
{
     
      grid.arrange(
          # header
          rectGrob(gp = gpar(fill = color, col = color)), # saves space for the header
          
          rectGrob(gp = gpar(fill = "slategray", col = "white")), 
          # first row of results   
          arrangeGrob(
               rectGrob(gp = gpar(fill = "white", col="white")),
               rectGrob(gp = gpar(fill = "white", col="white")),
               create_overall_condition(data,state,year,population),
               rectGrob(gp = gpar(fill = "white", col = "white")),
               nrow = 1,widths=unit(c(0.167,2,10.833,0.33),units="inches")
          ),
          rectGrob(gp = gpar(fill = "gray70", col = "white")), 
          rectGrob(gp = gpar(fill = "white", col = "white")), 
          
          # titles for the next set of results
          arrangeGrob(
               rectGrob(gp = gpar(fill = "white", col = "white")),
               textGrob("",just="right",gp=gpar(fontsize=12, col="Black")),
               textGrob("51 - Faulting (SP)",just="centre",gp=gpar(fontsize=10, col="slategray",fontface="bold")),
               textGrob("52 - Cracking Percent (SP)",just="centre",gp=gpar(fontsize=10, col="slategray",fontface="bold")),
               textGrob("50 - Rutting (SP)",just="centre",gp=gpar(fontsize=10, col="slategray",fontface="bold")),
               textGrob("47 - International Roughness Index (47)",just="centre",gp=gpar(fontsize=10, col="slategray",fontface="bold")),
               rectGrob(gp = gpar(fill = "white", col = "white")),
               nrow=1,widths=unit(c(0.167,1.5,rep(11.336/4,4),0.33),units="inches")
          ),
          # first row of tables
          arrangeGrob(
               rectGrob(gp = gpar(fill = "white", col = "white")),
               rectGrob(gp = gpar(fill = "white", col = "white")),
               create_table(
                    create_outlier_report(data,state,year,"FAULTING")),
               create_table(
                    create_outlier_report(data,state,year,"CRACKING_PERCENT")),
               create_table(
                    create_outlier_report(data,state,year,"RUTTING")),
               create_table(
                    create_outlier_report(data,state,year,"IRI")),
               rectGrob(gp = gpar(fill = "white", col = "white")),
               nrow = 1,widths=unit(c(0.167,1.5,rep(11.336/4,4),0.33),units="inches")),
          # second row of tables
          arrangeGrob(
               rectGrob(gp = gpar(fill = "white", col = "white")),
               rectGrob(gp = gpar(fill = "white", col = "white")),
               create_table(
                    create_adjacency_report(data,state,year,"FAULTING")),
               create_table(
                    create_adjacency_report(data,state,year,"CRACKING_PERCENT")),
               create_table(
                    create_adjacency_report(data,state,year,"RUTTING")),
               create_table(
                    create_adjacency_report(data,state,year,"IRI")),
               rectGrob(gp = gpar(fill = "white", col = "white")),
               nrow = 1,widths=unit(c(0.167,1.5,rep(11.336/4,4),0.33),units="inches")),
          # third row of results   
          arrangeGrob(
               rectGrob(gp = gpar(fill = "white", col = "white")),
               rectGrob(gp = gpar(fill = "white", col = "white")),
               create_table(
                    create_yearoveryear_report(data,state,year,"FAULTING",year_compare)),
               create_table(
                    create_yearoveryear_report(data,state,year,"CRACKING_PERCENT",year_compare)),
               create_table(
                    create_yearoveryear_report(data,state,year,"RUTTING",year_compare)),
               create_table(
                    create_yearoveryear_report(data,state,year,"IRI",year_compare)),
               rectGrob(gp = gpar(fill = "white", col = "white")),
               nrow = 1,widths=unit(c(0.167,1.5,rep(11.336/4,4),0.33),units="inches")),
          nrow=9,heights = unit(c(0.6,0.03,1.25,0.03,0.1,0.25,rep(5.24/3,3)),units="inches"))
     
     add_header(state,year,"pavement: detailed review","p")

     grid.text("Overall Condition",x=0.12,y=0.87,hjust=1,gp=gpar(fontsize=10, col="slategray"))
     grid.text("Represented as percent\nof total lane miles",x=0.12,y=0.83,hjust=1,gp=gpar(fontsize=8, col="slategray"))
     
     grid.text("Outliers",hjust=1,x=0.12,y=0.67,gp=gpar(fontsize=10, col="slategray"))
     grid.text("Sections\nwith outlying values",x=0.12,y=0.63,hjust=1,gp=gpar(fontsize=7, fontface="italic",col="slategray"))
     
     grid.text("Adjacent Sections",x=0.12,y=0.445,hjust=1,gp=gpar(fontsize=10, col="slategray"))
     grid.text("Adjacent sections\nwith the same value",x=0.12,y=0.405,hjust=1,gp=gpar(fontsize=7, fontface="italic",col="slategray"))

     grid.text(paste0(year_compare,"-",year," Sections"),x=0.12,y=0.22,hjust=1,gp=gpar(fontsize=10, col="slategray"))
     grid.text("Sections\nwith the same value\nas previous year",x=0.12,y=0.18,hjust=1,gp=gpar(fontsize=7, fontface="italic",col="slategray"))

     
     add_page_number(2)
     
     
     
}