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

  if ( exists('gTimeliness') ){
    
    deadline = ymd_hms(paste0(submission_deadline, ' 23:59:59'))
    
    sub_date = gTimeliness[State_Code == state & Year_Record == year, max(Submitted_On)]
    sub_date = mdy_hm(sub_date)
    
    score = 1 * (sub_date < deadline)
    
  } else {
    
    deadline = format(ymd(submission_deadline), '%m/%d/%Y 23:59:59')
    
    con <- odbcConnect("HPMS")
    
    score <- sqlQuery(con,
      paste0("select case when max(submitted_on) > '", deadline,
        "' then 0 else 1 end from dbo.[",timelinesstable,"] where State_Code = ",state," and Year_Record = ", year))
    
    score = score[1, 1]
    
    odbcClose(con)
  }
  
  return(score)
}
