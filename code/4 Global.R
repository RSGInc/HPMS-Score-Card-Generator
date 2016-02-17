###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2015
# Author: Jeff Keller & Jeff Dumont
#
#
# Description:
#
# This script creates/loads objects that must be globally accessible. This
# should be reserved for static objects whose definitions ideally will
# never change, or would change very infrequently and with the help of a
# programmer (e.g., if FHWA's logo changes).
#
###########################################################################

# Load additional fonts
#font.add.google("EB Garamond", "Garamond")

title_text <- "Lorem ipsum dolor sit amet, consectetur adipiscing\nelit. Donec lobortis massa a eros imperdiet\nvolutpat. Aenean sed ex eu leo accumsan tempus\neu mi. In hac habitasse platea dictumst. Quisque\ninterdum tellus a massa luctus euismod. In\nfringilla tincidunt turpis at egestas. Maecenas\nfaucibus augue eu tellus pulvinar, non maximus\npellentesque. Aliquam eget turpis nisi.\nSuspendisse imperdiet pharetra erat nec ornare.\nNulla lacinia, nisl euismod lacinia ultrices, est\nlorem ultricies felis, quis cursus magna tellus\neget justo."

# State Labels & Codes
gState_Labels <- data.table(
  index = c(1,2,4,5,6,8,9,10,11,12,13,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,44,45,46,47,48,49,50,51,53,54,55,56,60,66,69,72,78),
  abbr = c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS","KY",
           "LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR",
           "PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY","AS","GU","MP","PR","VI"),
  label = c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware",
            "District of Columbia","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa",
            "Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota",
            "Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey",
            "New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon",
            "Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah",
            "Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming","American Samoa",
            "Guam","Northern Mariana Islands","Puerto Rico","Virgin Islands of the U.S.")
  )

# FHWA Logo
gLogo  <- suppressWarnings(readPNG("resources/img/FHWA_vertical_2013.png"))
gLogo2 <- suppressWarnings(readPNG("resources/img/DVC-Logo.png"))
gLogo3 <- suppressWarnings(readPNG("resources/img/info.png"))

# Number of blank lines between menu question text
gSpaces <- 100


#######################################################################
# F system names for better reporting
#######################################################################
gF_SYSTEM_levels <- c("Interstate",
                      "Non-Interstate NHS",
                      "Other/Minor Arterials",
                      "Collectors + Locals"
)

gVariables       <- data.table(read.table("resources\\dat\\data_elements.csv",sep=",",header=TRUE,stringsAsFactors=FALSE))
gVariablesLabels <- data.table(read.table("resources\\dat\\data_labels.csv",  sep=",",header=TRUE,stringsAsFactors=FALSE))

gVariablesLabels[Name=="F_SYSTEM",Code2:="Principal Arterial -\nOther Freeways and Expressways"]
gVariablesLabels[Name=="F_SYSTEM",Code3:="Principal Arterial -\nOther"]
gVariablesLabels[Name=="F_SYSTEM",Code4:="Minor\nArterial"]
gVariablesLabels[Name=="F_SYSTEM",Code5:="Major\nCollector"]
gVariablesLabels[Name=="F_SYSTEM",Code6:="Minor\nCollector"]

gVariablesLabels[Name=="ACCESS_CONTROL",Code1:="Full\nAccess\nControl"]
gVariablesLabels[Name=="ACCESS_CONTROL",Code2:="Partial\nAccess\nControl"]
gVariablesLabels[Name=="ACCESS_CONTROL",Code3:="No\nAccess\nControl"]

NoDataString <- "Data item may not be required\nfor this State, follow up with\nState DOT and FHWA to confirm."