###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: RSG, Inc.
#
#
# Description:
#
# Welcome to the FHWA HPMS Score Card Generator! The HPMS Scorecard is a 
# product of the Office of Highway Policy Information and was developed
# by FHWA's Data Visualization Center. The Scorecard is a visually-oriented 
# statistical review of the HPMS data for any one of the 52 reporting 
# geographies (50 states plus Washington DC and Puerto Rico). The primary 
# purpose of this Scorecard is to provide a consistent and thorough tool 
# for review of the HPMS Data Items (also known as 'Section' or attribute 
# data).
#
# Instructions:
# Please see the HPMS Scorecard User's Manual for detailed instructions
# on the use of this code.
#
###########################################################################

options(scipen=9999)  # Make sure numbers are not converted to sci notation

submission_deadline = '2021-06-15'   # Deadline
# submission_deadline = '2020-06-15' # Usual deadline

# Load Code
codefiles = c(Sys.glob('app/*.R'), Sys.glob('functions/*.R'))
invisible(sapply(X =codefiles , FUN = source))

# Run Tool
# Run()  # Import all states
# updateNation(years = as.numeric(str_extract(submission_deadline, '^[0-9]{4}')) - 2)

# Run scorecards via command line tool.
# Rscript RunBatch.R ALL

debugmode = TRUE # Displays output to graphics device instead of PDF
# debug(plot_completeness)

# Import data
# Run(task = 1, state = 'DC', year = 2020)

# Run scorecard
Run(task = 2, state = 'TX', year = 2020, year_compare = 2019)

