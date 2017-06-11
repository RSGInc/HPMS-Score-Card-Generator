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


# Load Code
# setwd (needed when not using RStudio project)
# setwd('C:/Users/matt.landis/Git/HPMS-Score-Card-Generator')
invisible(sapply(X = list.files(path = "code", pattern = "*.R$",
                                full.names = TRUE)[-1], FUN = source))
# For debugging
# options(warn=2)
debugmode <- TRUE
# Run(task=1, state_selection='NJ', year_selection='2015')
Run(task=2, state_selection='RI', year_selection='2015', year_compare='2014')
# Run Tool
Run()

# Some responses for convenience

2
RI
2015
2014

2
NY
2015
2014

2
CA
2015
2014

2
AK
2015
2014

2
AZ
2015
2014

2
AR
2015
2014


2
CO
2015
2014

2
CT
2015
2014

2
DE
2015
2014

2
DC
2015
2014

2
FL
2015
2014

2
GA
2015
2014

2
HI
2015
2014

###############
2
ID
2015
2014

2
IL
2015
2014

2
IA
2015
2014

2
KS
2015
2014

2
KY
2015
2014

2
LA
2015
2014

2
ME
2015
2014

2
MD
2015
2014

2
MA
2015
2014

2
MI
2015
2014

2
MN
2015
2014

2
MS
2015
2014

2
MO
2015
2014

2
MT
2015
2014

2
NV
2015
2014

2
NH
2015
2014

2
NJ
2015
2014

2
NM
2015
2014

2
NY
2015
2014

2
NC
2015
2014

2
ND
2015
2014

2
OH
2015
2014

2
OK
2015
2014

2
OR
2015
2014

2
PA
2015
2014

2
RI
2015
2014

2
SC
2015
2014

2
SD
2015
2014

2
TN
2015
2014

2
TX
2015
2014

2
UT
2015
2014

2
VT
2015
2014

2
VA
2015
2014

2
WA
2015
2014

2
WV
2015
2014

2
WI
2015
2014

2
WY
2015
2014

2
CA
2015
2014


