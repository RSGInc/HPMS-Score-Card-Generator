###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Keller
#
#
# Description:
#
# This script points the tool to the local package repository, loads all
# required packages, and downloads them to the local repository if needed.
# All packages needed for the tool should be listed here and should not
# be called via 'library' anywhere else in the tool.
#
###########################################################################


### Begin loading tool components message
cat("Loading tool components")

# Uncomment this line to use the packages stored in the tool itself
# Be careful - if the library path is set to the tool's library, install.packages
# will install to this location, potentially overwriting the tool's libraries.
# This should be turned on for testing purposes and when the tool is sent to FHWA
# so they don't need to download anything.

#.libPaths("resources/lib")

# Function to auto-load packages (will attempt to download if not found)
LoadPackages <- function(packages) {
  
  notinstalled <- packages[!packages %in% .packages(all.available = TRUE)]
  if (length(notinstalled) > 0){
    install.packages(notinstalled,
                     repos = "http://cloud.r-project.org/", dependencies = TRUE)
  }
  for (package in packages) {
    cat(".")
    eval(parse(text = paste0("suppressMessages(library(", package, "))")),
         envir=.GlobalEnv)
  }
}

# Load necessary packages (alphabetically, please)
packages <- c("data.table", "doBy", "ggplot2", "gmodels",  "gridExtra",
              "hexbin", "jsonlite", "png", "reshape", "reshape2",
              "RODBC","scales", "showtext", "sqldf", 'stringr',"spatstat","reldist",
              "lubridate") #"grid","tcltk", "tools"
LoadPackages(packages)
library(tools)
library(grid)
library(tcltk)
#options(sqldf.driver = "SQLite")
