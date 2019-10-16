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

# Set memory limit to use virtual memory
invisible(memory.limit(64000))  # Set memory limit to 64 Gb

# Load Code
invisible(sapply(X = list.files(path = "code", pattern = "*.R$",
                                full.names = TRUE)[-1], FUN = source))

# Run Tool
# Run()

debugmode = TRUE
# debug(create_pdf)
Run(task = 2, state = 'CA', year = 2018, year_compare = 2017)

