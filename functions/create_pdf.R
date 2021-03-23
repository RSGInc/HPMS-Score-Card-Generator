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
  # cat('\tMemory used: ', round(mem_used() / 1e9, 3), 'GB \n')
  
  cat(paste(getStateLabelFromNum(state), "-", year, "vs.", year_compare, 
            "\n\n"))
  
  # Score card file name and path
  pdfname <- paste0(getStateAbbrFromNum(state), "_A", year, "_C", year_compare, 
                    "_", format(Sys.time(), "%Y%m%d_%k%M%S"), ".pdf")
  pdfname <- gsub(x = pdfname, pattern = "\\s", replace = "_")
  pdfpath <- paste0(path, pdfname)
  
  if (! dir.exists(path) ){
    dir.create(path) 
  }
  
  if (debugmode) {
    windows(width = 13.333, height = 7.5)
  } else {
    pdf(file = pdfpath, width = 13.333, height = 7.5)
  }
  
  # showtext.begin() # this controls the issues with the fonts

  # Create title page -------------------------------------------------------

  cat("Title page...")
  ts <- Sys.time()
  scores_list <- create_title_page(data, state, year, year_compare)
  cat(paste0(" completed in: ",
             round(difftime(Sys.time(), ts, units = "secs"), 2), " seconds!\n"))
  # cat('\tMemory used: ', round(mem_used() / 1e9, 3), 'GB \n')
  #
  #
  # Cross-validation -------------------------------------------------------
  #
  # Show the results of the cross-validations
  cat('Cross-validations...')
  ts <- Sys.time()
  create_cross_validation_page(scores_list$cross_validation, state, year)

  cat(paste0(' completed in: ',
             round(difftime(Sys.time(), ts, units='secs'), 2), ' seconds!\n'))
  # cat('\tMemory used: ', round(mem_used() / 1e9, 3), 'GB \n')

  # subset data
  # data <- data[year_record %in% c(year, year_compare), ]
  #

  # Information page --------------------------------------------------------

  cat("Information page...")
  ts <- Sys.time()
  create_info_page(state, year)
  cat(paste0(" completed in: ",
             round(difftime(Sys.time(), ts, units = "secs"), 2), " seconds!\n"))
  # cat('\tMemory used: ', round(mem_used() / 1e9, 3), 'GB \n')


  # Pavement: Detailed Review ------------------------------------------------

  cat("Pavement: Detailed Review...")
  ts <- Sys.time()
  create_pavement_detailed_review(data, state, year, year_compare)#, population = population)
  cat(paste0(" completed in: ",
             round(difftime(Sys.time(), ts, units = "secs"), 2), " seconds!\n"))
  # cat('\tMemory used: ', round(mem_used() / 1e9, 3), 'GB \n')


  # Traffic: Detailed Review --------------------------------------------------

  cat("Traffic: Detailed Review...")
  ts <- Sys.time()
  create_traffic_detailed_review(data, state, year, year_compare)
  cat(paste0(" completed in: ",
             round(difftime(Sys.time(), ts, units = "secs"), 2), " seconds!\n"))
  # cat('\tMemory used: ', round(mem_used() / 1e9, 3), 'GB \n')


  # Ramps: Detailed Review ----------------------------------------------------

  cat("Ramps: Detailed Review...")
  ts <- Sys.time()

  todo_vec <- (1:nrow(gVariables))[gVariables[, RampAnalysis] == "Y"]
  todo_vec <- c(todo_vec, rep(NA, 3 - (length(todo_vec) %% 3)))
  todo <- matrix(todo_vec, ncol = 3, byrow = TRUE)
  for (i in 1:1) {
    x1 <- todo[i, 1]
    x2 <- todo[i, 2]
    x3 <- todo[i, 3]

    gPageNumber <<- gPageNumber + 1

    create_page_summary(data, state, year, year_compare,
                        x1 = x1, x2 = x2, x3 = x3,
                        title = "ramps: detailed review", icontext = "r",
                        page = gPageNumber, ramps = TRUE)

    cat(".")
  }

  gPageNumber <<- gPageNumber + 1
  create_page_summary(data, state, year, year_compare,
                      x1 = todo[i + 1, 1], x2 = todo[i + 1, 2],
                      title = "ramps: detailed review", icontext = "r",
                      page = gPageNumber, ramps = TRUE)
  cat(paste0(" completed in: ",
             round(difftime(Sys.time(), ts, units = "secs"), 2), " seconds!\n"))
  # cat('\tMemory used: ', round(mem_used() / 1e9, 3), 'GB \n')


  # Inventory -----------------------------------------------------------------

  cat("Inventory...")
  ts <- Sys.time()

  todo_vec <- (1:nrow(gVariables))[gVariables[, Grouping] == "I"]
  todo_vec <- c(todo_vec, rep(NA, 3 - (length(todo_vec) %% 3)))
  todo <- matrix(todo_vec, ncol = 3, byrow = TRUE)
  for (i in 1:6) {

    x1 <- todo[i, 1]
    x2 <- todo[i, 2]
    x3 <- todo[i, 3]

    gPageNumber <<- gPageNumber + 1
    create_page_summary(data, state, year, year_compare,
                        x1 = x1, x2 = x2, x3 = x3,
                        title = "inventory", icontext = "i", page = gPageNumber)
    cat(".")
  }
  cat(paste0(" completed in: ", round(difftime(Sys.time(), ts, units = "secs"),
                                      2), " seconds!\n"))
  # cat('\tMemory used: ', round(mem_used() / 1e9, 3), 'GB \n')


  # Pavement -----------------------------------------------------------------

  cat("Pavement data items...")
  ts <- Sys.time()
  todo_vec <- (1:nrow(gVariables))[gVariables[, Grouping] == "P"]
  todo_vec <- c(todo_vec, rep(NA, 3 - (length(todo_vec) %% 3)))
  todo <- matrix(todo_vec, ncol = 3, byrow = TRUE)
  for (i in 1:4) {
    x1 <- todo[i, 1]
    x2 <- todo[i, 2]
    x3 <- todo[i, 3]

    gPageNumber <<- gPageNumber + 1
    create_page_summary(data, state, year, year_compare,
                        x1 = x1, x2 = x2, x3 = x3, title = "pavement",
                        icontext = "p", page = gPageNumber)
    cat(".")
  }
  gPageNumber <<- gPageNumber + 1
  create_page_summary(data, state, year, year_compare,
                      x1 = todo[i + 1, 1], title = "pavement", icontext = "p",
                      page = gPageNumber)
  cat(paste0(" completed in: ",
             round(difftime(Sys.time(), ts, units = "secs"), 2), " seconds!\n"))
  # cat('\tMemory used: ', round(mem_used() / 1e9, 3), 'GB \n')


  # Traffic -------------------------------------------------------------------

  cat("Traffic data items...")
  ts <- Sys.time()
  todo_vec <- (1:nrow(gVariables))[gVariables[, Grouping] == "T"]
  todo_vec <- c(todo_vec, rep(NA, 3 - (length(todo_vec) %% 3)))
  todo <- matrix(todo_vec, ncol = 3, byrow = TRUE)

  for (i in 1:4) {
    x1 <- todo[i, 1]
    x2 <- todo[i, 2]
    x3 <- todo[i, 3]

    gPageNumber <<- gPageNumber + 1
    create_page_summary(data, state, year, year_compare,
                        x1 = x1, x2 = x2,  x3 = x3,
                        title = "traffic", icontext = "t", page = gPageNumber)
    cat(".")
  }

  gPageNumber <<- gPageNumber + 1
  create_page_summary(data, state, year, year_compare,
                      x1 = todo[i +  1, 1],
                      x2 = todo[i + 1, 2],
                      title = "traffic", icontext = "t", page = gPageNumber)
  cat(paste0(" completed in: ", round(difftime(Sys.time(), ts, units = "secs"),
                                      2), " seconds!\n"))
  # cat('\tMemory used: ', round(mem_used() / 1e9, 3), 'GB \n')


  # Geometric -----------------------------------------------------------------

  cat("Geometric data items...")

  ts <- Sys.time()
  todo_vec <- (1:nrow(gVariables))[gVariables[, Grouping] == "G"]
  todo_vec <- c(todo_vec, rep(NA, 3 - (length(todo_vec) %% 3)))
  todo <- matrix(todo_vec, ncol = 3, byrow = TRUE)
  for (i in 1:7) {
    x1 <- todo[i, 1]
    x2 <- todo[i, 2]
    x3 <- todo[i, 3]

    gPageNumber <<- gPageNumber + 1
    create_page_summary(data, state, year, year_compare,
                        x1 = x1, x2 = x2, x3 = x3, title = "geometric",
                        icontext = "g", page = gPageNumber)
    cat(".")
  }

  gPageNumber <<- gPageNumber + 1
  create_page_summary(data, state, year, year_compare,
                      x1 = todo[i + 1, 1], x2 = todo[i + 1, 2],
                      title = "geometric", icontext = "g", page = gPageNumber)
  cat(paste0(" completed in: ",
             round(difftime(Sys.time(), ts, units = "secs"), 2), " seconds!\n"))


  # Route ---------------------------------------------------------------------

  cat("Route data items...")
  ts <- Sys.time()
  todo_vec <- (1:nrow(gVariables))[gVariables[, Grouping] == "R"]
  todo_vec <- c(todo_vec, rep(NA, 3 - (length(todo_vec) %% 3)))
  todo <- matrix(todo_vec, ncol = 3, byrow = TRUE)
  for (i in 1:1) {
    x1 <- todo[i, 1]
    x2 <- todo[i, 2]
    x3 <- todo[i, 3]

    gPageNumber <<- gPageNumber + 1
    create_page_summary(data, state, year, year_compare,
                        x1 = x1, x2 = x2, x3 = x3,
                        title = "route", icontext = "r", page = gPageNumber)
    cat(".")
  }
  cat(paste0(" completed in: ",
             round(difftime(Sys.time(), ts, units = "secs"), 2), " seconds!\n"))
  # cat('\tMemory used: ', round(mem_used() / 1e9, 3), 'GB \n')


  # Special network ----------------------------------------------------------

  cat("Special network data items...")
  ts <- Sys.time()
  todo_vec <- (1:nrow(gVariables))[gVariables[, Grouping] == "SN"]
  todo_vec <- c(todo_vec, rep(NA, 3 - (length(todo_vec) %% 3)))
  todo <- matrix(todo_vec, ncol = 3, byrow = TRUE)
  for (i in 1:1) {
    x1 <- todo[i, 1]
    x2 <- todo[i, 2]
    x3 <- todo[i, 3]

    gPageNumber <<- gPageNumber + 1
    create_page_summary(data, state, year, year_compare,
                        x1 = x1, x2 = x2, x3 = x3,
                        title = "special network", icontext = "sn",
                        page = gPageNumber)
    cat(".")
  }

  gPageNumber <<- gPageNumber + 1
  create_page_summary(data, state, year, year_compare,
                      x1 = todo[i + 1, 1], title = "special network", icontext = "sn",
                      page = gPageNumber)
  cat(paste0(" completed in: ",
             round(difftime(Sys.time(), ts, units = "secs"), 2), " seconds!\n"))
  # cat('\tMemory used: ', round(mem_used() / 1e9, 3), 'GB \n')

  # showtext.end()

  if ( !debugmode ){
    dev.off()
  }
  
  gPageNumber <<- 1
  # cat('\tMemory used: ', round(mem_used() / 1e9, 3), 'GB \n')
  cat('Exiting create_pdf -----------------------------------')
  whitespace(4)
  
}