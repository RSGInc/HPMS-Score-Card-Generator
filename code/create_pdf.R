###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont
#
#
# Description:
#
# This function creates the score card PDF file output.
#
###########################################################################

create_pdf <- function(data, state, year, year_compare, national = NULL, path) {
  TS <- Sys.time()
  
  cat("Creating score card...\n\n")
  cat(paste(getStateLabelFromNum(state), "-", year, "vs.", year_compare, 
            "\n\n"))
  
  # Score card file name and path
  pdfname <- paste0(getStateAbbrFromNum(state), "_A", year, "_C", year_compare, 
                    "_", format(Sys.time(), "%Y%m%d_%k%M%S"), ".pdf")
  pdfname <- gsub(x = pdfname, pattern = "\\s", replace = "_")
  pdfpath <- paste0(path, pdfname)
  
  if (debugmode) {
    windows(width = 13.333, height = 7.5)
  } else {
    pdf(file = pdfpath, width = 13.333, height = 7.5)
  }
  
  # showtext.begin() # this controls the issues with the fonts
  
  # Create title page
  cat("Title page...")
  ts <- Sys.time()
  create_title_page(data, state, year, year_compare)
  cat(paste0("\nTitle page completed in: ",
             round(difftime(Sys.time(), ts, units = "secs"), 2), " seconds!\n"))

  # subset data
  data <- data[year_record %in% c(year, year_compare), ]

  cat("Information page...")
  ts <- Sys.time()
  create_info_page(state, year)
  cat(paste0(" completed in: ",
             round(difftime(Sys.time(), ts, units = "secs"), 2), " seconds!\n"))

  # Pavement: Detailed Review
  cat("Pavement review...")
  ts <- Sys.time()
  create_page2(data, state, year, year_compare, population = population)
  cat(paste0(" completed in: ",
             round(difftime(Sys.time(), ts, units = "secs"), 2), " seconds!\n"))

  # Traffic: Detailed Review
  cat("Traffic review...")
  ts <- Sys.time()
  create_page3(data, state, year, year_compare)
  cat(paste0(" completed in: ",
             round(difftime(Sys.time(), ts, units = "secs"), 2), " seconds!\n"))
  
  # Ramps: Detailed Review
  cat("Ramps review...")
  ts <- Sys.time()
  todo <- matrix((1:nrow(gVariables))[gVariables[, RampAnalysis] == "Y"], 
                 ncol = 3, byrow = TRUE)
  for (i in 1:1) {
    x1 <- todo[i, 1]
    x2 <- todo[i, 2]
    x3 <- todo[i, 3]
    
    create_page_summary(data, state, year, year_compare,
                        x1 = x1, x2 = x2, x3 = x3, 
                        title = "ramps: detailed review", icontext = "r", 
                        page = 3 + i, ramps = TRUE)
    cat(".")
  }
  create_page_summary(data, state, year, year_compare,
                      x1 = todo[i + 1, 1], x2 = todo[i + 1, 2],
                      title = "ramps: detailed review", icontext = "r", 
                      page = 5, ramps = TRUE)
  cat(paste0(" completed in: ",
             round(difftime(Sys.time(), ts, units = "secs"), 2), " seconds!\n"))
  
  
  # data summary pages inventory
  cat("Inventory data items...")
  ts <- Sys.time()
  todo <- matrix((1:nrow(gVariables))[gVariables[, Grouping] == "I"], 
                 ncol = 3, byrow = TRUE)
  for (i in 1:6) {
    x1 <- todo[i, 1]
    x2 <- todo[i, 2]
    x3 <- todo[i, 3]
    create_page_summary(data, state, year, year_compare,
                        x1 = x1, x2 = x2, x3 = x3,
                        title = "inventory", icontext = "i", page = 5 + i)
    cat(".")
  }
  cat(paste0(" completed in: ", round(difftime(Sys.time(), ts, units = "secs"), 
                                      2), " seconds!\n"))
  
  # Pavement
  cat("Pavement data items...")
  ts <- Sys.time()
  todo <- matrix((1:nrow(gVariables))[gVariables[, Grouping] == "P"], 
                 ncol = 3, byrow = TRUE)
  for (i in 1:4) {
    x1 <- todo[i, 1]
    x2 <- todo[i, 2]
    x3 <- todo[i, 3]
    create_page_summary(data, state, year, year_compare,
                        x1 = x1, x2 = x2, x3 = x3, title = "pavement",
                        icontext = "p", page = 11 + i)
    cat(".")
  }
  create_page_summary(data, state, year, year_compare,
                      x1 = todo[i + 1, 1], title = "pavement", icontext = "p",
                      page = 12 + i)
  cat(paste0(" completed in: ",
             round(difftime(Sys.time(), ts, units = "secs"), 2), " seconds!\n"))
  
  # Traffic
  cat("Traffic data items...")
  ts <- Sys.time()
  todo <- matrix((1:nrow(gVariables))[gVariables[, Grouping] == "T"], 
                 ncol = 3, byrow = TRUE)
  for (i in 1:4) {
    x1 <- todo[i, 1]
    x2 <- todo[i, 2]
    x3 <- todo[i, 3]
    create_page_summary(data, state, year, year_compare,
                        x1 = x1, x2 = x2,  x3 = x3,
                        title = "traffic", icontext = "t", page = 16 + i)
    cat(".")
  }
  create_page_summary(data, state, year, year_compare,
                      x1 = todo[i +  1, 1],
                      x2 = todo[i + 1, 2],
                      title = "traffic", icontext = "t", page = 17 + i)
  cat(paste0(" completed in: ", round(difftime(Sys.time(), ts, units = "secs"), 
                                      2), " seconds!\n"))
  
  # Geometric
  cat("Geometric data items...")
  ts <- Sys.time()
  todo <- matrix((1:nrow(gVariables))[gVariables[, Grouping] == "G"], 
                 ncol = 3, byrow = TRUE)
  for (i in 1:7) {
    x1 <- todo[i, 1]
    x2 <- todo[i, 2]
    x3 <- todo[i, 3]
    create_page_summary(data, state, year, year_compare,
                        x1 = x1, x2 = x2, x3 = x3, title = "geometric", icontext = "g", page = 21 + i)
    cat(".")
  }
  create_page_summary(data, state, year, year_compare,
                      x1 = todo[i + 1, 1], x2 = todo[i + 1, 2],
                      title = "geometric", icontext = "g", page = 29)
  cat(paste0(" completed in: ",
             round(difftime(Sys.time(), ts, units = "secs"), 2), " seconds!\n"))
  
  # Route
  cat("Route data items...")
  ts <- Sys.time()
  todo <- matrix((1:nrow(gVariables))[gVariables[, Grouping] == "R"], 
                 ncol = 3, byrow = TRUE)
  for (i in 1:1) {
    x1 <- todo[i, 1]
    x2 <- todo[i, 2]
    x3 <- todo[i, 3]
    create_page_summary(data, state, year, year_compare,
                        x1 = x1, x2 = x2, x3 = x3,
                        title = "route", icontext = "r", page = 29 + i)
    cat(".")
  }
  cat(paste0(" completed in: ",
             round(difftime(Sys.time(), ts, units = "secs"), 2), " seconds!\n"))
  
  # Special network
  cat("Special network data items...")
  ts <- Sys.time()
  todo <- matrix((1:nrow(gVariables))[gVariables[, Grouping] == "SN"], 
                 ncol = 3, byrow = TRUE)
  for (i in 1:1) {
    x1 <- todo[i, 1]
    x2 <- todo[i, 2]
    x3 <- todo[i, 3]
    create_page_summary(data, state, year, year_compare,
                        x1 = x1, x2 = x2, x3 = x3,
                        title = "special network", icontext = "sn",
                        page = 30 + i)
    cat(".")
  }
  create_page_summary(data, state, year, year_compare, x1 = todo[i + 
                                                                   1, 1], title = "special network", icontext = "sn", page = 32)
  cat(paste0(" completed in: ",
             round(difftime(Sys.time(), ts, units = "secs"), 2), " seconds!\n"))
  
  # showtext.end()
  dev.off()
  
  whitespace(4)
}