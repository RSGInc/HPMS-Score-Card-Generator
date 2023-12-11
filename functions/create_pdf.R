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

create_pdf <- function(data, state, year, year_compare, path) {
  
  TS <- Sys.time()

  message(
    "Creating score card for ", 
    paste(getStateLabelFromNum(state), "-", year, "vs.", year_compare, "\n\n"))
  
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
    on.exit(expr = dev.off())
  
  }
  
  # showtext.begin() # this controls the issues with the fonts

  # Create title page -------------------------------------------------------

  message("Title page...")
  ts <- Sys.time()
  scores_list <- create_title_page(data, state, year, year_compare)
  message(paste0(" completed in: ",
             round(difftime(Sys.time(), ts, units = "secs"), 2), " seconds!\n"))
  # cat('\tMemory used: ', round(mem_used() / 1e9, 3), 'GB \n')
  #
  #
  # Cross-validation -------------------------------------------------------
  #
  # Show the results of the cross-validations
  message('Cross-validations...')
  ts <- Sys.time()
  browser()
  # test adding multiple xval pages
  #row_cutoff = as.integer( nrow(scores_list$cross_validation)/2 )
  row_cutoff = 42
  
  cv_list1 = scores_list$cross_validation[1:row_cutoff]
  cv_list2 = scores_list$cross_validation[(row_cutoff + 1):nrow(scores_list$cross_validation)]
  
  #create_cross_validation_page(scores_list$cross_validation, state, year)
  create_cross_validation_page(cv_list1, state, year)
  create_cross_validation_page(cv_list2, state, year)

  message(paste0(' completed in: ',
             round(difftime(Sys.time(), ts, units='secs'), 2), ' seconds!\n'))
  # cat('\tMemory used: ', round(mem_used() / 1e9, 3), 'GB \n')

  # subset data
  # data <- data[datayear %in% c(year, year_compare), ]
  #

  # Documentation page ------------------------------------------------------

  message('Documentation page ...')
  create_documentation_pages(state, year)


  # Pavement: Detailed Review ------------------------------------------------

  message("Pavement: Detailed Review...")
  ts <- Sys.time()
  create_pavement_detailed_review(data, state, year, year_compare)#, population = population)
  message(paste0(" completed in: ",
             round(difftime(Sys.time(), ts, units = "secs"), 2), " seconds!\n"))
  # cat('\tMemory used: ', round(mem_used() / 1e9, 3), 'GB \n')


  # Traffic: Detailed Review --------------------------------------------------

  message("Traffic: Detailed Review...")
  ts <- Sys.time()
  create_traffic_detailed_review(data, state, year, year_compare)
  message(paste0(" completed in: ",
             round(difftime(Sys.time(), ts, units = "secs"), 2), " seconds!\n"))
  # cat('\tMemory used: ', round(mem_used() / 1e9, 3), 'GB \n')


  # Ramps: Detailed Review ----------------------------------------------------

  message("Ramps: Detailed Review...")
  ts <- Sys.time()

  todo_vec <- (1:nrow(gVariables))[gVariables[, RampAnalysis] == "Y"]
  todo_vec <- c(todo_vec, rep(NA, 3 - (length(todo_vec) %% 3)))
  todo <- matrix(todo_vec, ncol = 3, byrow = TRUE)
  todo <- todo[rowSums(!is.na(todo)) > 0, , drop = FALSE]
  
  for (i in 1:nrow(todo)) {
    x1 <- todo[i, 1]
    x2 <- todo[i, 2]
    x3 <- todo[i, 3]

    gPageNumber <<- gPageNumber + 1

    create_page_summary(data, state, year, year_compare,
                        x1 = x1, x2 = x2, x3 = x3,
                        title = "ramps: detailed review", icontext = "r",
                        page = gPageNumber, ramps = TRUE)

    
  }

  message(paste0(" completed in: ",
             round(difftime(Sys.time(), ts, units = "secs"), 2), " seconds!\n"))
  # cat('\tMemory used: ', round(mem_used() / 1e9, 3), 'GB \n')


  # Inventory -----------------------------------------------------------------

  message("Inventory...")
  ts <- Sys.time()

  todo_vec <- (1:nrow(gVariables))[gVariables[, Grouping] == "I"]
  todo_vec <- c(todo_vec, rep(NA, 3 - (length(todo_vec) %% 3)))
  todo <- matrix(todo_vec, ncol = 3, byrow = TRUE)
  todo <- todo[rowSums(!is.na(todo)) > 0, , drop = FALSE]
  
  for (i in 1:nrow(todo)) {
    
    x1 <- todo[i, 1]
    x2 <- todo[i, 2]
    x3 <- todo[i, 3]

    browser()
    
    if (FALSE)
      
      #item_num_tll = gVariables[Name == 'TURN_LANES_L', Item_Number]
      
      # Make sure TURN_LANES_L and TURN_LANES_R are on the same page
      idx_tll = gVariables[Name == 'TURN_LANES_L', which = TRUE]
      idx_tlr = gVariables[Name == 'TURN_LANES_R', which = TRUE]
      
      row_tll = ceiling(match(idx_tll, todo_vec) / 3)
      row_tlr = ceiling(match(idx_tlr, todo_vec) / 3)
      
      if (row_tll != row_tlr) {
        ## re-arrange
      }
    }
    
    gPageNumber <<- gPageNumber + 1
    create_page_summary(data, state, year, year_compare,
                        x1 = x1, x2 = x2, x3 = x3,
                        title = "inventory", icontext = "i", page = gPageNumber)
  }
  message(paste0(" completed in: ", round(difftime(Sys.time(), ts, units = "secs"),
                                      2), " seconds!\n"))
  # cat('\tMemory used: ', round(mem_used() / 1e9, 3), 'GB \n')


  # Pavement -----------------------------------------------------------------

  message("Pavement data items...")
  ts <- Sys.time()
  todo_vec <- (1:nrow(gVariables))[gVariables[, Grouping] == "P"]
  todo_vec <- c(todo_vec, rep(NA, 3 - (length(todo_vec) %% 3)))
  todo <- matrix(todo_vec, ncol = 3, byrow = TRUE)
  todo <- todo[rowSums(!is.na(todo)) > 0, , drop = FALSE]
  
  for (i in 1:nrow(todo)) {
    x1 <- todo[i, 1]
    x2 <- todo[i, 2]
    x3 <- todo[i, 3]

    gPageNumber <<- gPageNumber + 1
    create_page_summary(data, state, year, year_compare,
                        x1 = x1, x2 = x2, x3 = x3, title = "pavement",
                        icontext = "p", page = gPageNumber)
  }

  message(paste0(" completed in: ",
                 round(difftime(Sys.time(), ts, units = "secs"), 2), " seconds!\n"))
  # cat('\tMemory used: ', round(mem_used() / 1e9, 3), 'GB \n')


  # Traffic -------------------------------------------------------------------

  message("Traffic data items...")
  ts <- Sys.time()
  todo_vec <- (1:nrow(gVariables))[gVariables[, Grouping] == "T"]
  todo_vec <- c(todo_vec, rep(NA, 3 - (length(todo_vec) %% 3)))
  todo <- matrix(todo_vec, ncol = 3, byrow = TRUE)
  todo <- todo[rowSums(!is.na(todo)) > 0, , drop = FALSE]
  
  for (i in 1:nrow(todo)) {
    x1 <- todo[i, 1]
    x2 <- todo[i, 2]
    x3 <- todo[i, 3]

    gPageNumber <<- gPageNumber + 1
    create_page_summary(data, state, year, year_compare,
                        x1 = x1, x2 = x2,  x3 = x3,
                        title = "traffic", icontext = "t", page = gPageNumber)
    
  }

  message(paste0(" completed in: ", round(difftime(Sys.time(), ts, units = "secs"),
                                      2), " seconds!\n"))
  # cat('\tMemory used: ', round(mem_used() / 1e9, 3), 'GB \n')


  # Geometric -----------------------------------------------------------------

  message("Geometric data items...")

  ts <- Sys.time()
  todo_vec <- (1:nrow(gVariables))[gVariables[, Grouping] == "G"]
  todo_vec <- c(todo_vec, rep(NA, 3 - (length(todo_vec) %% 3)))
  todo <- matrix(todo_vec, ncol = 3, byrow = TRUE)
  todo <- todo[rowSums(!is.na(todo)) > 0, , drop = FALSE]
  
  for (i in 1:nrow(todo)) {
    x1 <- todo[i, 1]
    x2 <- todo[i, 2]
    x3 <- todo[i, 3]

    gPageNumber <<- gPageNumber + 1
    create_page_summary(data, state, year, year_compare,
                        x1 = x1, x2 = x2, x3 = x3, title = "geometric",
                        icontext = "g", page = gPageNumber)
    
  }

  message(paste0(" completed in: ",
             round(difftime(Sys.time(), ts, units = "secs"), 2), " seconds!\n"))


  # Route ---------------------------------------------------------------------

  message("Route data items...")
  ts <- Sys.time()
  todo_vec <- (1:nrow(gVariables))[gVariables[, Grouping] == "R"]
  todo_vec <- c(todo_vec, rep(NA, 3 - (length(todo_vec) %% 3)))
  todo <- matrix(todo_vec, ncol = 3, byrow = TRUE)
  todo <- todo[rowSums(!is.na(todo)) > 0, , drop = FALSE]
  
  for (i in 1:nrow(todo)) {
    x1 <- todo[i, 1]
    x2 <- todo[i, 2]
    x3 <- todo[i, 3]

    gPageNumber <<- gPageNumber + 1
    create_page_summary(data, state, year, year_compare,
                        x1 = x1, x2 = x2, x3 = x3,
                        title = "route", icontext = "r", page = gPageNumber)
    
  }
  message(paste0(" completed in: ",
             round(difftime(Sys.time(), ts, units = "secs"), 2), " seconds!\n"))
  # cat('\tMemory used: ', round(mem_used() / 1e9, 3), 'GB \n')


  # Special network ----------------------------------------------------------

  message("Special network data items...")
  ts <- Sys.time()
  todo_vec <- (1:nrow(gVariables))[gVariables[, Grouping] == "SN"]
  todo_vec <- c(todo_vec, rep(NA, 3 - (length(todo_vec) %% 3)))
  todo <- matrix(todo_vec, ncol = 3, byrow = TRUE)
  todo <- todo[rowSums(!is.na(todo)) > 0, , drop = FALSE]
  
  for (i in 1:nrow(todo) ) {
    x1 <- todo[i, 1]
    x2 <- todo[i, 2]
    x3 <- todo[i, 3]

    gPageNumber <<- gPageNumber + 1
    create_page_summary(data, state, year, year_compare,
                        x1 = x1, x2 = x2, x3 = x3,
                        title = "special network", icontext = "sn",
                        page = gPageNumber)
    
  }

  message(paste0(" completed in: ",
             round(difftime(Sys.time(), ts, units = "secs"), 2), " seconds!\n"))
  # cat('\tMemory used: ', round(mem_used() / 1e9, 3), 'GB \n')

  # showtext.end()

  gPageNumber <<- 1
  # cat('\tMemory used: ', round(mem_used() / 1e9, 3), 'GB \n')
  message('Exiting create_pdf -----------------------------------')
  whitespace(4)
  
}