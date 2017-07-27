###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Keller and Jeff Dumont
# Modified: Matt Landis
#
# Description:
#
# This set of code controls the import of the data from the SQL database
# into the scorecard. It is important for the ODBC connection to be set up
# correctly for this to work. Please see the user manual for more details
# on how to set up the connection.
#
###########################################################################

goverwrite <- ""

# List available states and years
showAvailableStatesYears <- function(){
  
  # Get states and years available for import -----------------------------
  con <- odbcConnect("HPMS")
  
  cat('Determining available states and years...\n')
  
  # FHWA
  #data1 <- data.table(sqlQuery(con,paste0("select distinct state_code, year_record from sections2015 order by state_code,year_record")))
  #data2 <- data.table(sqlQuery(con,paste0("select distinct state_code, year_record from sections2014 order by state_code,year_record")))
  #data3 <- data.table(sqlQuery(con,paste0("select distinct state_code, year_record from sections2013 order by state_code,year_record")))
  
  #data <- rbind(data1,data2)#,data3)
  
  # RSG
  query <- paste("select distinct state_code, year_record from", sections_table,
                 "order by state_code, year_record")
  
  data <- data.table(sqlQuery(con, query))
  
  odbcClose(con)
  
  # Print available states and years
  
  cat("Data available for import include:\n")
  
  for(state in unique(data[,state_code])){
    
    cat(paste0(getStateAbbrFromNum(state),": "))
    
    for(year in data[state_code==state,unique(year_record)]){
      
      cat(paste0(year," "))  
      
    }
    cat("\n")
  }
  
  cat("\n\n")
  return(data)
}

askStates <- function(data){
  # Ask which state -----------------------------------------------------
  
  state = ""
  invalidresponse = TRUE
  while(invalidresponse){
    
    state <- getUserInput(prompt="What state(s) would you like to import?\nEnter 2 letter abbreviations separated by commas.\nTo import all states, type ALL.")
    
    invalidresponse = FALSE
    
    for(st in strsplit(state,",")[[1]]){
      
      if(!(st %in% c("ALL",sapply(unique(data[,state_code]),getStateAbbrFromNum)))){
        invalidresponse = TRUE
      }
    }
    
    if(invalidresponse){
      cat("Invalid response:",st)
    }
    
  }
  
  if(state=="ALL")  {
    codes <- unique(data[,state_code])
    states <- c()
    
    for(code in codes){
      
      states <- c(states, getStateAbbrFromNum(code))
      
    }
    
  } else {
    states <- strsplit(state,",")[[1]]
  }
  return(states)  
}

askYears <- function(data){
  
  # Ask which year -----------------------------------------------------------
  
  year <- ""
  
  ranges <- merge(unique(data[,year_record]),unique(data[,year_record]))
  ranges <- ranges[ranges[,1]<ranges[,2],]
  ranges <- paste0(ranges[,1],"-",ranges[,2])
  
  validyears <- c("ALL",unique(data[,year_record]),ranges)
  
  while(!(year %in% validyears)){
    
    year  <- getUserInput(prompt="\nWhat year(s) would you like to import?\nEnter a single year or a range as in 2012-2014.\nFor all years, type ALL.")
    
    if(!(year %in% validyears))
    {
      cat("Invalid response:",year)  
    }
  }
  
  if(year=="ALL")  {
    
    years <- unique(data[,year_record])
    
  } else {
    
    years  <- strsplit(year,"-")[[1]]
    
    if(length(years)>1){
      
      years <- (years[1]):(years[2])
      
    }
  }
  return(years)  
}


# This function runs the entire data import process

ImportData <- function(state_selection, year_selection) {
  success <- c()
  
  if ( missing(state_selection) | missing(year_selection) ){
    st_yr <- showAvailableStatesYears()
    
    states <- askStates(st_yr)
    years <- askYears(st_yr)
    
  } else {
    states <- state_selection
    years <- year_selection
  }
  
  # Load the data -----------------------------------------------------------
  
  for (state in states) {
    
    for(year in years) {
      
      # Create filename to save to
      
      state_name <- getStateLabel(state)
      state_code <- getStateNumFromCode(state)
      
      path <- file.path("data", state_name)
      file <- paste0(year, ".RDS")
      fullpath <- file.path(path, file)
      
      # See if a data set for this year and state already exists
      okayToSave <- TRUE
      
      if ( file.exists(fullpath) ) {
        # File exists, check with user
        if(!(goverwrite %in% c("ALL Y","ALL N"))){
          
          overwrite <- getUserInput(valid = c("Y", "y", "N", "n","ALL Y","ALL N"), prompt = paste0("Processed file already exists for ", state, " ", year, ". Overwrite it? (Y, N, ALL Y, ALL N): "))
        } 
        
        if(goverwrite == "ALL Y"){
          overwrite <- "Y"
        }
        
        if(goverwrite == "ALL N"){
          overwrite <- "N"
        }
        
        if(overwrite=="ALL Y"){
          goverwrite <<- overwrite
          overwrite <- "Y"
        } 
        
        if(overwrite=="ALL N"){
          goverwrite <<- overwrite
          overwrite <- "N"
        }  
        
        if (!tolower(overwrite) == "y"){
          okayToSave <- FALSE
        }
      }
      
      if ( !okayToSave ) next()
      
      # Read data
      cat(paste0("\nProcessing ", state," (",year, ")...\n"))
      
      data <- ReadData(state, year)
      
      if (is.null(data)|nrow(data)==0) {
        success <- c(success, FALSE)
      } else {
        
        # Check data sets
 
        cat("Checking imported data ...")
        
        passedChecks <- CheckImport(year=year, state_code=state_code, dat=data)
        
        if ( !passedChecks ){
          warning(state, '(', year, ') failed check.  Not imported.\n')
          next()
        }
        
        cat(" complete!")
        
        # # Chop imported data into year-state combinations
        # data <- SegmentDataSet(data)

        # # Check data sets
        # cat("Checking imported data ...")
        
        # passedChecks <- unlist(lapply(X = data, FUN = function(x) CheckImport(year = x[["year"]], state = x[["state"]], dat = x[["data"]])))
        # data[!passedChecks] <- NULL
        
        # if(length(data)>0){
        
        # Format data sets
        cat("\nFormatting...")
        
        data <- FormatDataSet(dat=data, state_abbr=state, year=year)
        cat(" complete!\n")
        
        #for (i in 1:length(data)){
        
        #data[[i]][["data"]] <- FormatDataSet(data[[i]][["data"]],state,year)
        
        #}
        
        
        # Save data sets ------------------
        
        cat("Saving to", fullpath, '...')
        
        # Create new directory if needed
        if (!dir.exists(path)) dir.create(path)
        
        saveRDS(data, file = fullpath)
        
        # for (i in 1:length(data)){
        #   
        #   SaveDataSet(year = data[[i]][["year"]],
        #               state = data[[i]][["state"]],
        #               dat = data[[i]][["data"]])
        # }   
        
        cat(" complete!\n")
        
        #} # if (length(data) > 0)
        
        success <- c(success, TRUE)
        
      }
      
    } # year
  } # state
  
  goverwrite <<- "" # reset
  return(any(success))
} 

cleanUpQuery <- function(data){
  # Converts data table from sqlQuery for use in R

  # make names lower case
  names(data) <- tolower(names(data))

  # if data_item is present, make upper case
  if('data_item' %in% names(data)){
    data$data_item <- toupper(data$data_item)
  }

  # Strip white space from text columns so things join properly
  # Also make upper case
  chr_idx <- which(sapply(data, is.character))
  for(j in chr_idx){
    data[, j] <- str_trim(data[, j], side='both')
    data[, j] <- toupper(data[, j])
  }
 
  # Convert to a data.table
  data <- data.table(data)

  return(data)
}

  
# Attempt to import a data file specified by the user
ReadData <- function(state, year) {

  cat('Fetching the data from the database...')
  con <- odbcConnect("HPMS")

  query <- paste0('select * from ', sections_table, ' where StateYearKey = ',
                  getStateNumFromCode(state), as.numeric(year) %% 100)
  
  data <- sqlQuery(con, query, stringsAsFactors=FALSE)
  
  odbcClose(con)

  data <- cleanUpQuery(data)
  
  cat('complete!\n')
  
  return(data)
  
}

# If an imported data contains data for multiple states and/or years, break it into multiple data sets
SegmentDataSet <- function(dat) {
  
  dat.list <- list()
  datasets <- unique(dat[, .(year_record, state_code)])
  
  cc <- 1
  
  for (i in 1:datasets[, .N]) {
    
    year <- datasets[i, year_record]
    state <- datasets[i, state_code]
    dat.list[[cc]] <- list("year" = year,
                           "state" = state,
                           "data" = dat[year_record == year & state_code == state])
    
    cc <- cc + 1
    
  }
  
  return(dat.list)
  
}

# Create a column for a particular data_item from value_numeric
transposeItem <- function(dfname, data_item){

    sql <- paste0('select A.*, B.value_numeric as ', data_item, ' ',
               'from [', dfname, '] A ',
               'left join [', dfname, '] B on A.route_id = B.route_id and ',
               'A.year_record = B.year_record and ',
               'A.state_code = B.state_code and ',
               'A.begin_point <= B.end_point and ',
               'A.begin_point >= B.begin_point and ',
               'A.end_point <= B.end_point and ',
               'A.end_point >= B.begin_point and ',
               "B.data_item = '", data_item, "'")
  
  return(sql)  
}

# Format a given data set for use in the score card generation process
FormatDataSet <- function(dat, state_abbr, year) {

  
  
  # Merge data on itself to convert rows to columns
  
  data.formatted <- data.table( sqldf(transposeItem('dat', 'F_SYSTEM') ))
  data.formatted <- data.table( sqldf(transposeItem('data.formatted', 'NHS')) )
  data.formatted <- data.table( sqldf(transposeItem('data.formatted', 'FACILITY_TYPE')))
  data.formatted <- data.table( sqldf(transposeItem('data.formatted', 'THROUGH_LANES')))
  data.formatted <- data.table( sqldf(transposeItem('data.formatted', 'URBAN_CODE')))

  #F_SYSTEM Codes
  # 1 Interstate
  # 2 Principal Arterial - Other Freeways and Expressways
  # 3 Principal Arterial - Other
  # 4 Minor Arterial
  # 5 Major Collector
  # 6 Minor Collector
  # 7 Local
  
  # Recode Interstate, NHS, and F_SYSTEM variables
  data.formatted[, F_SYTEMorig := F_SYSTEM]
  data.formatted[, Interstate := c(1,0,0,0,0,0,0)[F_SYSTEM]]
  data.formatted[, NHS := c(1,0,0,0,0,0,0)[NHS]]
  data.formatted[, NHS := NHS * (1 - Interstate)]
  data.formatted[, F_SYSTEM := c(NA,1,1,1,2,2,2)[F_SYSTEM]]
  
  # remove non-inventory data but keep for summary
  data_noFT6 <- data.formatted[FACILITY_TYPE != 6, ]
  data_FT6 <- data.formatted[FACILITY_TYPE == 6 | is.na(FACILITY_TYPE), ]
  
  # merge in expansion factors ---------------------------------------------

  #sp <- data.table(read.table(spfile,sep="|",header=TRUE,stringsAsFactors=FALSE))

  con <- odbcConnect("HPMS")

  # query <- paste0("select YEAR_RECORD as YEAR_RECORD, STATE_CODE as STATE_CODE, ROUTE_ID as ROUTE_ID, BEGIN_POINT as BEGIN_POINT, END_POINT as END_POINT, SAMPLE_ID as SAMPLE_ID, EXPANSION_FACTOR as EXPANSION_FACTOR from samples",year," where state_code = ",getStateNumFromCode(state)," and year_record = ",year)
  
  query <- paste0('select * from ', samples_table, ' where StateYearKey = ',
                  getStateNumFromCode(state_abbr), as.numeric(year) %% 100)
  sp <- sqlQuery(con, query)
  
  odbcClose(con)

  sp <- cleanUpQuery(sp)
    
  # setnames(sp,
  #         c("YEAR_RECORD", "STATE_CODE", "ROUTE_ID","BEGIN_POINT", "END_POINT", "SAMPLE_ID","EXPANSION_FACTOR"),
  #         c("year_record", "state_code", "route_id","begin_point", "end_point", "sample_id","expansion_factor"))
  
  data_exp <- data.table(
    sqldf("select A.*, B.expansion_factor as expansion_factor
           from [data_noFT6] A 
           left join [sp] B on A.route_id = B.route_id and 
                                           A.year_record = B.year_record and 
                                           A.state_code = B.state_code and 
                                           A.begin_point <= B.end_point and 
                                           A.begin_point >= B.begin_point and 
                                           A.end_point <= B.end_point and 
                                           A.end_point >= B.begin_point"))
  
  data_exp[, expansion_factor:=as.numeric(expansion_factor)]
  rm(data.formatted)
  

  
  # Check formatted data against the FHWA summary
  # Create a dataset to compare (need to bindrows the FT6 data)
  keep_cols <- c('year_record', 'state_code', 'data_item', 'route_id', 'section_length')
  for_comp <- rbindlist(
    list(data_exp[, keep_cols, with=FALSE], data_FT6[, keep_cols, with=FALSE]),
    use.names=TRUE)
  
  # Check imported data against summary table -------------------------------
  state_code <- getStateNumFromCode(state_abbr)
  
  check <- checkSummary(year, state_code, dat)
  
  if ( !isTRUE(check) ){
   
    # Save the mismatches
    path <- file.path('data', getStateLabelFromNum(state_code))
    file <- paste0(year, '_summary_fail_on_formatting.csv')
    fullpath <- file.path(path, file)
    
    # Create new directory if needed
    if (!dir.exists(path)) dir.create(path)
    
    # Write the file
    write.csv(x=check, file=fullpath, na='', row.names=FALSE)
   
    warntext <- paste(year, getStateAbbrFromNum(state_code),
                      'failed summary check.  Saving diffs to',
                      fullpath, '\n')
    warning(warntext)
  }
  

  # Prepare to write out the data
  setkeyv(data_exp, c("year_record", "route_id", "data_item"))

  rm(data_noFT6)  
  gc()
  
  return(data_exp)
  
}

