###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Keller & Jeff Dumont
# Modified: Matt Landis
#
# Description:
#
# This script creates/loads objects that must be globally accessible. This
# should be reserved for static objects whose definitions ideally will
# never change, or would change very infrequently and with the help of a
# programmer (e.g., if FHWA's logo changes).
#
###########################################################################

# Change the ggplot theme
ggplot2::theme_update(plot.title=element_text(hjust=0.5))

# SQL table names
# these need to match FHWA's sql database structure
#timelinesstable  <- "Table_mssql_m_HPMSAnalysis_HPMS_Submissions_log" 
timelinesstable   <- "Timelinesstable"   # "2015SubmissionDates"
sections_table    <- 'ReviewSections'
samples_table     <- 'ReviewSampleSections'
db_username       <- ''
db_password       <- ''

# These don't seem to be used anywhere...
# datatable        <- "sections"
# sampletable      <- "samples"
# summary_table     <- 'Section_Summaries'
# poptable          <- "codes_urban"

GetODBCConnection <- function() {
  if(db_username == '' | db_password == '') {
    return(odbcConnect("HPMS"))
  } else {
    return(odbcConnect("HPMS", uid = db_username, pwd = db_password))
  }
}

# this is the text included in the left side of the first page of the scorecard
# carriage returns are necessary.
title_text <- 
"The HPMS Scorecard is a product of the Office 
of Highway Policy Information and was developed 
by FHWA's Data Visualization Center. The Scorecard 
is a visually oriented statistical review of the 
HPMS data for any one of the 52 reporting 
geographies (50 states plus Washington DC and Puerto 
Rico). The primary purpose of this Scorecard is to 
provide a consistent and thorough tool for review 
of the HPMS Data Items (also known as 'Section' 
or attribute data). 

The Scorecard is intended to serve as a visual tool 
to highlight areas of concern or data irregularities, 
but is not an exhaustive error finding tool. The 
Scorecard reflects data from  1) the 'Analysis Year'
which is typically  the most recent data year and 
2) a previous  'Comparison Year', which is required 
to accommodate many of the Scorecard's temporal 
calculations. The Scorecard also evaluates the National 
patterns for the year prior to the Analysis Year. 
Elements of the Scorecard are 1) statewide data 
timeliness, quality and completeness summary, 
2) information on the interpretation of scorecard 
elements, 3) pavement and travel items detailed reviews,
4) ramp data details and 5) HPMS Data Item statistical 
review."

# Should we run in debugmode?
debugmode <- FALSE


# State Labels & Codes
gState_Labels <- fread('resources/dat/state_labels.csv')

# Colors

gColors <- list(
  blank = 'white',
  text = 'black',
  dark = 'slategray',
  light = 'gray75',
  accent = 'steelblue4',
  text_background = 'gray90',
  highlight = 'red'
)

# FHWA and DVC Logos
gLogo  <- suppressWarnings(readPNG("resources/img/FHWA_vertical_2013.png"))
gLogo2 <- suppressWarnings(readPNG("resources/img/DVC-Logo.png"))
gInfoPage <- suppressWarnings(readPNG("resources/img/info.png"))
gScorePage <- suppressWarnings(readPNG('resources/img/score_info.png'))

# Number of blank lines between menu question text
gSpaces <- 2


#######################################################################
# F system names for better reporting
#######################################################################
gF_SYSTEM_levels <- c("Interstate",
                      "Non-Interstate NHS",
                      "Other/Minor Arterials",
                      "Collectors + Locals"
)

# tables of variables (data items) and labels used in the output
gVariables       <- fread("resources/dat/data_elements.csv")
gVariablesLabels <- fread("resources/dat/data_labels.csv")
gCrossLabels     <- fread('resources/dat/cross_validation_labels.csv')
gCrossLabels$Description <- str_replace(gCrossLabels$Description, '[(][0-9xy]*[)]$', '')
gExtentDetail    <- fread('resources/dat/extent_detail.csv')
gReqs <- fread("resources/dat/data_items_required_by_state.csv")
gScoreWeights <- fread("resources/dat/scoringweights.csv")

timeliness_file = file.path('data/timeliness_table.csv')


if ( file.exists( timeliness_file )){
  gTimeliness = fread(timeliness_file)
}

# reformatting the labels
gVariablesLabels[
  Name=="F_SYSTEM", 
  Code2:="Principal Arterial -\nOther Freeways and Expressways"]

gVariablesLabels[
  Name=="F_SYSTEM",
  Code3:="Principal Arterial -\nOther"]

gVariablesLabels[
  Name=="F_SYSTEM",
  Code4:="Minor\nArterial"]

gVariablesLabels[
  Name=="F_SYSTEM",
  Code5:="Major\nCollector"]

gVariablesLabels[Name=="F_SYSTEM",
  Code6:="Minor\nCollector"]

gVariablesLabels[Name=="ACCESS_CONTROL",
  Code1:="Full\nAccess\nControl"]

gVariablesLabels[Name=="ACCESS_CONTROL",
  Code2:="Partial\nAccess\nControl"]

gVariablesLabels[Name=="ACCESS_CONTROL",
  Code3:="No\nAccess\nControl"]

# track page number
gPageNumber <- 1

# this is the output if there is a problem with any analysis of a data item
NoDataString <- "Data item may not be required\nfor this State, follow up with\nState DOT and FHWA to confirm."
