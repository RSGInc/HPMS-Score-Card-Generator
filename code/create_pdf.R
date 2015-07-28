###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2015
# Author: Jeff Dumont
#
#
# Description:
#
# This function creates the score card PDF file output.
#
###########################################################################

# TODO: needs to accept an argument pointing to where the file should be saved.
create_pdf <- function(data,state,year,year_compare, population)
{
     pdf(file = paste0(gState_Labels[index==state,label],"_",year,"_",year_compare,".pdf"), width = 13.333, height = 7.5)
     
     showtext.begin() # this controls the issues with the fonts
     
     # first page
     create_title_page(data,state,year,year_compare)
     cat("***** Finished creating title page\n")
     
     # second page
     create_page2(data,state,year,year_compare,population=population)
     cat("***** Finished creating page 2\n")
     
     # third page and on
     create_page3(data,state,year,year_compare)
     cat("***** Finished creating page 3\n")
     
     # create summary pages organize by type
     # I	19 mod 3 = 1
     # R	3        = 0 
     # T	16       = 1
     # G	23       = 2
     # P	14       = 1
     # SN	4        = 1
     # O	2        = 2
     
     # inventory
     todo <- matrix((1:81)[gVariables[,Grouping]=="I"],ncol=3,byrow=TRUE)
     for(i in 1:6)
     {
          x1 <- todo[i,1]
          x2 <- todo[i,2]
          x3 <- todo[i,3]
          create_page_summary(data,state,year,year_compare,x1=x1,x2=x2,x3=x3, title="inventory",icontext="i")
     }
     create_page_summary(data,state,year,year_compare,x1=todo[i+1,1],title="inventory",icontext="i")
     cat("***** Finished creating inventory data items\n")
     
     # Pavement
     todo <- matrix((1:81)[gVariables[,Grouping]=="P"],ncol=3,byrow=TRUE)
     for(i in 1:4)
     {
          x1 <- todo[i,1]
          x2 <- todo[i,2]
          x3 <- todo[i,3]
          create_page_summary(data,state,year,year_compare,x1=x1,x2=x2,x3=x3, title="pavement",icontext="p")
     }
     create_page_summary(data,state,year,year_compare,x1=todo[i+1,1], title="pavement",icontext="p")
     catt("***** Finished creating pavement data items\n")

     # Traffic
     todo <- matrix((1:81)[gVariables[,Grouping]=="T"],ncol=3,byrow=TRUE)
     for(i in 1:5)
     {
          x1 <- todo[i,1]
          x2 <- todo[i,2]
          x3 <- todo[i,3]
          create_page_summary(data,state,year,year_compare,x1=x1,x2=x2,x3=x3, title="traffic",icontext="t")
     }
     create_page_summary(data,state,year,year_compare,x1=todo[i+1,1], title="traffic",icontext="t")
     cat("***** Finished creating traffic data items\n")     
     
     # Geometric
     todo <- matrix((1:81)[gVariables[,Grouping]=="G"],ncol=3,byrow=TRUE)
     for(i in 1:7)
     {
          x1 <- todo[i,1]
          x2 <- todo[i,2]
          x3 <- todo[i,3]
          create_page_summary(data,state,year,year_compare,x1=x1,x2=x2,x3=x3, title="geometric",icontext="g")
     }
     create_page_summary(data,state,year,year_compare,x1=todo[i+1,1],x2=todo[i+1,2], title="geometric",icontext="g")
     cat("***** Finished creating geometric data items\n")
     
     # Route
     todo <- matrix((1:81)[gVariables[,Grouping]=="R"],ncol=3,byrow=TRUE)
     for(i in 1:1)
     {
          x1 <- todo[i,1]
          x2 <- todo[i,2]
          x3 <- todo[i,3]
          create_page_summary(data,state,year,year_compare,x1=x1,x2=x2,x3=x3, title="route",icontext="r")
     }
     cat("***** Finished creating route data items\n")
     
     # Special network
     todo <- matrix((1:81)[gVariables[,Grouping]=="SN"],ncol=3,byrow=TRUE)
     for(i in 1:1)
     {
          x1 <- todo[i,1]
          x2 <- todo[i,2]
          x3 <- todo[i,3]
          create_page_summary(data,state,year,year_compare,x1=x1,x2=x2,x3=x3, title="special network",icontext="sn")
     }
     create_page_summary(data,state,year,year_compare,x1=todo[i+1,1], title="special network",icontext="sn")
     cat("***** Finished creating special network data items\n")
     
     
     #######
     # Other
     #todo <- matrix((1:81)[gVariables[,Grouping]=="O"]],ncol=3,byrow=TRUE)
     #create_page_summary(state,year,year_compare,x1=todo[1,1],x2=todo[1,2], color="#00B0F0",title="Other Data Items")
     #print("***** Finished creating other data items")
     
     showtext.end()
     dev.off()
     
     cat(paste0("***** ",gState_Labels[index==state,label]," PDF Complete\n"))
     
}
