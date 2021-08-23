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

create_table_grob <- function(result, variable_type){

  # Set the table theme
  thm <- ttheme_default(
    core    = list(fg_params=list(col=gColors$text, fontsize=4.5, hjust=1, x=0.95),
                   bg_params=list(fill='grey95'),
                   padding=unit(c(0.1, 0.1), 'inches')),
    colhead = list(fg_params=list(col=gColors$text, fontsize=4.0,
                                  fontface='bold', hjust=1, x=0.95),
                   bg_params=list(fill=gColors$text_background),
                   padding=unit(c(0.1, 0.1), 'inches')))
  
  # Rename groupCat levels
  result[,groupCat:=gF_SYSTEM_levels[as.numeric(result[,groupCat])]]
  
  # no longer printed these to save space
  result[,count:=NULL]
  result[,count.na:=NULL]
  result[,mean:=NULL]
  #result[,min:=NULL]
  #result[,max:=NULL]

  # Convert NAs to zeros -- if miles are NA, means that no sections were found
  # in that Funct System
  
  result[is.na(miles), miles := 0]
  result[is.na(expandedmiles), expandedmiles := 0]
  result[is.na(lanemiles), lanemiles := 0]
  result[is.na(expandedlanemiles), expandedlanemiles := 0]
  
  
  setnames(result,"groupCat","Functional\nSystem")
  #setnames(result,"count","N")
  #setnames(result,"count.na","N (NA)")
  setnames(result,"miles","Total \nCenterline Mi")
  setnames(result,"expandedmiles","Tot. Expanded\nCenterline Mi")
  setnames(result,"lanemiles","Total \nLane Mi")
  setnames(result,"expandedlanemiles","Tot. Expanded\nLane Mi")

  if(variable_type %in% c('numeric', 'date')){
    
    setnames(result,"min","Min")
    #setnames(result,"mean","Mean")
    setnames(result,"median","Median")
    setnames(result,"max","Max")
  }

  ob <- tableGrob(result, rows=NULL, theme=thm )
  
  ob <- vertically_align(ob)
  
  return(ob)
}
