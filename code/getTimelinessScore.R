###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont 
#
#
# Description:
#
# Function to get timeliness score from the FHWA's database. Used only on
# title page.
#
###########################################################################

getTimelinessScore <- function(state,year)
{

  con <- odbcConnect("HPMS")
  
  score <- sqlQuery(con,paste0("select case when max(submitted_on) > '6/16/",year+1,"' then 0 else 1 end from dbo.[",timelinesstable,"] where State_Code = ",state," and Year_Record = ", year))
  
  odbcClose(con)
  
  return(score)
}