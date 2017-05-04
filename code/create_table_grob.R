###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont
#
#
# Description:
#
# This function creates a generic table grob object to be used througout
# the score card, but primarily on the summary pages.
#
###########################################################################

create_table_grob <- function(result,variable_type)
{
  
  result[,groupCat:=gF_SYSTEM_levels[as.numeric(result[,groupCat])]]
  
  # no longer printed these to save space
  result[,count:=NULL]
  result[,count.na:=NULL]
  result[,mean:=NULL]
  #result[,min:=NULL]
  #result[,max:=NULL]

  setnames(result,"groupCat","Functional\nSystem")
  #setnames(result,"count","N")
  #setnames(result,"count.na","N (NA)")
  setnames(result,"miles","Total \nCenterline Miles")
  setnames(result,"expandedmiles","Total Expanded\nCenterline Miles")
  setnames(result,"lanemiles","Total \nLane Miles")
  setnames(result,"expandedlanemiles","Total Expanded\nLane Miles")

  if(variable_type < 3)
  {
    
    setnames(result,"min","Min")
    #setnames(result,"mean","Mean")
    setnames(result,"median","Median")
    setnames(result,"max","Max")
  }

  thm <- ttheme_default(
    core    = list(fg_params=list(col='black', fontsize=4.5, hjust=1, x=0.95),
                   bg_params=list(fill='grey95'),
                   padding=unit(c(0.1, 0.1), 'inches')),
    colhead = list(fg_params=list(col='black', fontsize=4.0,
                                  fontface='bold', hjust=1, x=0.95),
                   bg_params=list(fill='grey90'),
                   padding=unit(c(0.1, 0.1), 'inches')))

    ob <- tableGrob(result, rows=NULL, theme=thm )
    
    ob <- vertically_align(ob)
    
    return(ob)
  }
