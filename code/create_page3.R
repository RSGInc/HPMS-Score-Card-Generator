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

create_page3 <- function(
     data,
     state,
     year,
     year_compare=NULL,
     color="white"
     )
{
     grid.arrange(
          # header
          rectGrob(gp = gpar(fill = color, col = color)), # saves space for the header
          
          # line
          rectGrob(gp = gpar(fill = "slategray", col = "white")), 
          
          # buffer
          rectGrob(gp = gpar(fill = "white", col = "white")), 
          
          
          # titles for first row of results
          arrangeGrob(
               rectGrob(gp = gpar(fill = "white", col = "white")),
               textGrob("Extreme Future AADT Values",just="centre",gp=gpar(fontsize=10, col="slategray")),
               
               textGrob("Future AADT relative to AADT",just="centre",gp=gpar(fontsize=10, col="slategray")),
               textGrob("Combination Unit AADT relative to AADT",just="centre",gp=gpar(fontsize=10, col="slategray")),
               textGrob("Single Unit AADT relative to AADT",just="centre",gp=gpar(fontsize=10, col="slategray")),
               
               rectGrob(gp = gpar(fill = "white", col = "white")),
               nrow=1,widths=unit(c(1/6,2+1/3,rep((10+2/3)/3,3),1/6),units="inches")
          ),
          
          # first row of results
          arrangeGrob(
               rectGrob(gp = gpar(fill = "white", col = "white")),
               create_travel_data_outlier(data,state,year,year_compare),
               create_travel_data_assessment(data,state,year,year_compare),
               rectGrob(gp = gpar(fill = "white", col = "white")),
               
               nrow=1,widths=unit(c(1/6,2+1/3,(10+2/3),1/6),units="inches")
          ),
          
          # buffer
          rectGrob(gp = gpar(fill = "white", col = "white")),

          # divider
          rectGrob(gp = gpar(fill = "slategray", col = "slategray")),
          
          # buffer
          rectGrob(gp = gpar(fill = "white", col = "white")),
          
          # titles for the adjacency tables.               
          arrangeGrob(
               rectGrob(gp = gpar(fill = "white", col = "white")),
               textGrob("",just="right",gp=gpar(fontsize=12, col="Black")),
               textGrob("21 - Annual Average Daily Traffic (FE+R)",just="centre",gp=gpar(fontsize=10, col="slategray",fontface="bold")),
               textGrob("28 - Future AADT (SP)",just="centre",gp=gpar(fontsize=10, col="slategray",fontface="bold")),
               textGrob("24 - Combination Truck AADT (FE*)",just="centre",gp=gpar(fontsize=10, col="slategray",fontface="bold")),
               textGrob("25 - Single Unit Truck and Bus AADT (FE*)",just="centre",gp=gpar(fontsize=10, col="slategray",fontface="bold")),
               rectGrob(gp = gpar(fill = "white", col = "white")),
               nrow=1,widths=unit(c(0.167,1.5,rep(11.336/4,4),0.33),units="inches")
          ),               
          # other results
          arrangeGrob(
               rectGrob(gp = gpar(fill = "white", col = "white")),
               rectGrob(gp = gpar(fill = "white", col = "white")),
               create_table(
                    create_adjacency_report(data,state,year,"AADT")),
               create_table(
                    create_adjacency_report(data,state,year,"FUTURE_AADT")),
               create_table(
                    create_adjacency_report(data,state,year,"AADT_COMBINATION")),
               create_table(
                    create_adjacency_report(data,state,year,"AADT_SINGLE_UNIT")),
               rectGrob(gp = gpar(fill = "white", col = "white")),
               nrow = 1,widths=unit(c(0.167,1.5,rep(11.336/4,4),0.33),units="inches")),
          
          arrangeGrob(
               rectGrob(gp = gpar(fill = "white", col = "white")),
               rectGrob(gp = gpar(fill = "white", col = "white")),
               create_table(
                    create_yearoveryear_report(data,state,year,"AADT",year_compare)),
               create_table(
                    create_yearoveryear_report(data,state,year,"FUTURE_AADT",year_compare)),
               create_table(
                    create_yearoveryear_report(data,state,year,"AADT_COMBINATION",year_compare)),
               create_table(
                    create_yearoveryear_report(data,state,year,"AADT_SINGLE_UNIT",year_compare)),
               rectGrob(gp = gpar(fill = "white", col = "white")),
               nrow = 1,widths=unit(c(0.167,1.5,rep(11.336/4,4),0.33),units="inches")),
          
          # bottom buffer
          rectGrob(gp = gpar(fill = "white", col = "white")),
          
          nrow=12,heights=unit(c(0.6,0.03,0.15,0.25,2.12,0.16,0.01,0.16,0.25,0.7533333/2+2.75/2,0.7533333/2+2.75/2,0.2666667),units="inches"))
     
     add_header(state,year,"traffic: detailed review","t")
     
     grid.text("Adjacent Sections",x=0.125,y=0.49,hjust=1,gp=gpar(fontsize=10, col="slategray"))
     grid.text("Adjacent sections\nwith the same value",x=0.12,y=0.45,hjust=1,gp=gpar(fontsize=7, fontface="italic",col="slategray"))
     
     grid.text(paste0(year_compare,"-",year," Sections"),x=0.12,y=0.260,hjust=1,gp=gpar(fontsize=10, col="slategray"))
     grid.text("Sections\nwith the same value\nas previous year",x=0.12,y=0.22,hjust=1,gp=gpar(fontsize=7, fontface="italic",col="slategray"))
     
     add_page_number(3)
     
}