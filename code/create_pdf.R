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
create_pdf <- function(data, state, year, year_compare, population, national = NULL, path)
{
  
     cat("Creating score card...\n\n")
     cat(paste(getStateLabelFromNum(state), "-", year, "vs.", year_compare, "\n\n"))
     
     # Score card file name and path
     pdfname <- paste0(getStateLabelFromNum(state), "_A", year, "_C", year_compare, ".pdf")   
     pdfname <- gsub(x = pdfname, pattern = "\\s", replace = "_")
     pdfpath <- paste0(path, pdfname)
  
     pdf(file = pdfpath, width = 13.333, height = 7.5)
     
     showtext.begin() # this controls the issues with the fonts
     
     # Create title page
     cat("Title page...")
     create_title_page(data,state,year,year_compare)
     cat(" complete!\n")
     
     # Pavement: Detailed Review
     cat("Pavement review...")
     create_page2(data,state,year,year_compare,population=population)
     cat(" complete!\n")
     
     # Pavement: Detailed Review
     cat("Traffic review...")
     create_page3(data,state,year,year_compare)
     cat(" complete!\n")
     
     # create summary pages organize by type
     # I	19 mod 3 = 1
     # R	3        = 0 
     # T	16       = 1
     # G	23       = 2
     # P	14       = 1
     # SN	4        = 1
     # O	2        = 2
     
     # inventory
     cat("Inventory data items...")
     todo <- matrix((1:nrow(gVariables))[gVariables[,Grouping]=="I"],ncol=3,byrow=TRUE)
     for(i in 1:6)
     {
          x1 <- todo[i,1]
          x2 <- todo[i,2]
          x3 <- todo[i,3]
          create_page_summary(data,state,year,year_compare,x1=x1,x2=x2,x3=x3, title="inventory",icontext="i",page=3+i)
          cat(".")
     }
     #create_page_summary(data,state,year,year_compare,x1=todo[i+1,1],title="inventory",icontext="i",page=3+i+1)
     cat(" complete!\n")
     
     # Pavement
     cat("Pavement data items...")
     todo <- matrix((1:nrow(gVariables))[gVariables[,Grouping]=="P"],ncol=3,byrow=TRUE)
     for(i in 1:4)
     {
          x1 <- todo[i,1]
          x2 <- todo[i,2]
          x3 <- todo[i,3]
          create_page_summary(data,state,year,year_compare,x1=x1,x2=x2,x3=x3, title="pavement",icontext="p",page=10+i)
          cat(".")
     }
     create_page_summary(data,state,year,year_compare,x1=todo[i+1,1], title="pavement",icontext="p",page=11+i)
     cat(" complete!\n")

     # Traffic
     cat("Traffic data items...")
     todo <- matrix((1:nrow(gVariables))[gVariables[,Grouping]=="T"],ncol=3,byrow=TRUE)
     for(i in 1:4)
     {
          x1 <- todo[i,1]
          x2 <- todo[i,2]
          x3 <- todo[i,3]
          create_page_summary(data,state,year,year_compare,x1=x1,x2=x2,x3=x3, title="traffic",icontext="t",page=15+i)
          cat(".")
     }
     create_page_summary(data,state,year,year_compare,x1=todo[i+1,1],x2=todo[i+1,2], title="traffic",icontext="t",page=16+i)
     cat(" complete!\n")
     
     # Geometric
     cat("Geometric data items...")
     todo <- matrix((1:nrow(gVariables))[gVariables[,Grouping]=="G"],ncol=3,byrow=TRUE)
     for(i in 1:7)
     {
          x1 <- todo[i,1]
          x2 <- todo[i,2]
          x3 <- todo[i,3]
          create_page_summary(data,state,year,year_compare,x1=x1,x2=x2,x3=x3, title="geometric",icontext="g",page=21+i)
          cat(".")
     }
     create_page_summary(data,state,year,year_compare,x1=todo[i+1,1],x2=todo[i+1,2], title="geometric",icontext="g",page=29)
     cat(" complete!\n")
     
     # Route
     cat("Route data items...")
     todo <- matrix((1:nrow(gVariables))[gVariables[,Grouping]=="R"],ncol=3,byrow=TRUE)
     for(i in 1:1)
     {
          x1 <- todo[i,1]
          x2 <- todo[i,2]
          x3 <- todo[i,3]
          create_page_summary(data,state,year,year_compare,x1=x1,x2=x2,x3=x3, title="route",icontext="r",page=29+i)
          cat(".")
     }
     cat(" complete!\n")
     
     # Special network
     cat("Special network data items...")
     todo <- matrix((1:nrow(gVariables))[gVariables[,Grouping]=="SN"],ncol=3,byrow=TRUE)
     for(i in 1:1)
     {
          x1 <- todo[i,1]
          x2 <- todo[i,2]
          x3 <- todo[i,3]
          create_page_summary(data,state,year,year_compare,x1=x1,x2=x2,x3=x3, title="special network",icontext="sn",page=30+i)
          cat(".")
     }
     create_page_summary(data,state,year,year_compare,x1=todo[i+1,1], title="special network",icontext="sn",page=32)
     cat(" complete!\n")
     
     showtext.end()
     dev.off()
     
     whitespace(4)
     cat(paste0("Score card complete!\n\n", pdfname, " written to the output folder."))
     
}
