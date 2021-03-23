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

getTimelinessScore <- function(state, year,
                               submission_deadline = paste0(year + 1, '-06-16'))
{

  sub_date = format(ymd(submission_deadline), '%m/%d/%Y 23:59:00')
  con <- odbcConnect("HPMS")
  
  score <- sqlQuery(con,
                    paste0("select case when max(submitted_on) > '", sub_date,
                           "' then 0 else 1 end from dbo.[",timelinesstable,"] where State_Code = ",state," and Year_Record = ", year))
  
  odbcClose(con)
  
  return(score)
}