###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2015
# Author: Jeff Keller
#
#
# Description:
#
# Author needs to add a description!
#
###########################################################################

goverwrite <- ""

# This function runs the entire data import process
ImportFiles <- function() {
  
  success <- c()
  
  con <- odbcConnect("HPMS")
  
  # FHWA
  data1 <- data.table(sqlQuery(con,paste0("select distinct state_code, year_record from sections2014 order by state_code,year_record")))
  data2 <- data.table(sqlQuery(con,paste0("select distinct state_code, year_record from sections2013 order by state_code,year_record")))
  
  data <- rbind(data1,data2)
  
  # RSG
  #data <- data.table(sqlQuery(con,paste0("select distinct state_code, year_record from HPMSAnalysis order by state_code,year_record")))
  
  odbcClose(con)
  cat("Data available for import include:\n")
  for(state in unique(data[,state_code]))
  {
    cat(paste0(getStateAbbrFromNum(state),": "))
    for(year in data[state_code==state,unique(year_record)])
    {
      cat(paste0(year," "))  
    }
    cat("\n")
  }
  cat("\n\n")
  state = ""
  invalidresponse = TRUE
  while(invalidresponse)
  {
    state <- getUserInput(prompt="What state(s) would you like to import?\nEnter 2 letter abbreviations separated by commas.\nTo import all states, type ALL.")
    
    invalidresponse = FALSE
    
    for(st in strsplit(state,",")[[1]])
    {
      if(!(st %in% c("ALL",sapply(unique(data[,state_code]),getStateAbbrFromNum))))
      {
        invalidresponse = TRUE
      }
    }
    if(invalidresponse)
    {
      cat("Invalid response:",st)
    }
  }
  
  if(state=="ALL")  {
    codes <- unique(data[,state_code])
    states <- c()
    for(code in codes)
    {
      states <- c(states,getStateAbbrFromNum(code))
    }
  } else {
     states <- strsplit(state,",")[[1]]
  }
  
  year <- ""
  
  ranges <- merge(unique(data[,year_record]),unique(data[,year_record]))
  ranges <- ranges[ranges[,1]<ranges[,2],]
  ranges <- paste0(ranges[,1],"-",ranges[,2])

  validyears <- c("ALL",unique(data[,year_record]),ranges)
  
  while(!(year %in% validyears))
  {
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
    if(length(years)>1)
    {
      years <- (years[1]):(years[2])
    }
  }
  
  for (state in states) {
    
    for(year in years) {
      # Read data
      cat(paste0("\nProcessing ", state," (",year, ")...\n"))
      data <- ReadFile(state,year)
      
      setnames(data, 
               c("YEAR_RECORD", "STATE_CODE", "ROUTE_ID","BEGIN_POINT", "END_POINT", "DATA_ITEM","VALUE_NUMERIC", "VALUE_TEXT", "VALUE_DATE"),
               c("year_record", "state_code", "route_id","begin_point", "end_point", "data_item","value_numeric", "value_text", "value_date")
              )
      
      if (is.null(data)|nrow(data)==0) {
        success <- c(success, FALSE)
      } else {
        
        # Chop imported data into year-state combinations
        data <- SegmentDataSet(data)
        
        # Check data sets
        cat("Checking...")
        passedChecks <- unlist(lapply(X = data, FUN = function(x) CheckData(year = x[["year"]], state = x[["state"]], dat = x[["data"]])))
        data[!passedChecks] <- NULL
        cat(" complete!")
        
        if(length(data)>0)
        {
          # Format data sets
          cat("\nFormatting...")
          for (i in 1:length(data)) data[[i]][["data"]] <- FormatDataSet(data[[i]][["data"]],state,year)
          cat(" complete!")
          
          # Save data sets
          cat("\nSaving to disk...")
          for (i in 1:length(data)) SaveDataSet(year = data[[i]][["year"]], state = data[[i]][["state"]], dat = data[[i]][["data"]])
          cat(" complete!\n")
        
        }
        success <- c(success, TRUE)
        
      }
    } # year
  } # state
  
  goverwrite <<- "" # reset
  return(any(success))
}

# Attempt to import a data file specified by the user
ReadFile <- function(state,year) {
  
  # These are the expected columns and classes
  #colClasses <- c("Year_Record" = "integer", "State_Code" = "integer", "Route_ID" = "factor",
  #                "Begin_Point" = "numeric", "End_Point" = "numeric", "Data_Item" = "factor",
  #                "Value_Numeric" = "numeric", "Value_Text" = "character", "Value_Date" = "character")
  
  # Try to load the data
  #tryCatch(expr = {dat <- fread(file, colClasses = colClasses);
  #                 factorCols <- names(colClasses[colClasses == "factor"]);
  #                 dat[, (factorCols) := lapply(X = .SD, FUN = factor), .SDcols = factorCols];
  #                 return(dat)},
  #         error = function(e) {warning(paste(basename(file), "does not appear to be in the proper format. Skipping."), immediate. = TRUE, call. = FALSE); return(NULL)})
  
  con <- odbcConnect("HPMS")

  data <- sqlQuery(con,paste0("select * from sections",year," where state_code = ",getStateNumFromCode(state)," and year_record = ",year))
  
  odbcClose(con)
  
  return(data.table(data))
  
}

# If an imported data contains data for multiple states and/or years, break it into multiple data sets
SegmentDataSet <- function(dat) {
  
  dat.list <- list()
  datasets <- unique(dat[, .(year_record, state_code)])
  
  cc <- 1
  for (i in 1:datasets[, .N]) {
    year <- datasets[i, year_record]
    state <- datasets[i, state_code]
    dat.list[[cc]] <- list("year" = year, "state" = state, "data" = dat[year_record == year & state_code == state])
    cc <- cc + 1
  }
  
  return(dat.list)
  
}

# Format a given data set for use in the score card generation process
FormatDataSet <- function(dat,state,year) {
  
  cat(".")
  
  # Merge data on itself to get the F_SYSTEM variable
  data.formatted <- data.table(
    sqldf("select A.*, B.value_numeric as F_SYSTEM 
           from [dat] A 
           left join [dat] B on A.route_id = B.route_id and 
                                           A.year_record = B.year_record and 
                                           A.state_code = B.state_code and 
                                           A.begin_point <= B.end_point and 
                                           A.begin_point >= B.begin_point and 
                                           A.end_point <= B.end_point and 
                                           A.end_point >= B.begin_point and B.data_item = 'F_SYSTEM'"))
  
  # Merge data on itself to get the NHS variable
  data.formatted <- data.table(
    sqldf("select A.*, B.value_numeric as NHS 
           from [data.formatted] A 
           left join [data.formatted] B on A.route_id = B.route_id and 
                                           A.year_record = B.year_record and 
                                           A.state_code = B.state_code and 
                                           A.begin_point <= B.end_point and 
                                           A.begin_point >= B.begin_point and 
                                           A.end_point <= B.end_point and 
                                           A.end_point >= B.begin_point and B.data_item = 'NHS'"))

  data.formatted <- data.table(
    sqldf("select A.*, B.value_numeric as FACILITY_TYPE 
           from [data.formatted] A 
           left join [data.formatted] B on A.route_id = B.route_id and 
                                           A.year_record = B.year_record and 
                                           A.state_code = B.state_code and 
                                           A.begin_point <= B.end_point and 
                                           A.begin_point >= B.begin_point and 
                                           A.end_point <= B.end_point and 
                                           A.end_point >= B.begin_point and B.data_item = 'FACILITY_TYPE'"))

  data.formatted <- data.table(
    sqldf("select A.*, B.value_numeric as THROUGH_LANES 
           from [data.formatted] A 
           left join [data.formatted] B on A.route_id = B.route_id and 
                                           A.year_record = B.year_record and 
                                           A.state_code = B.state_code and 
                                           A.begin_point <= B.end_point and 
                                           A.begin_point >= B.begin_point and 
                                           A.end_point <= B.end_point and 
                                           A.end_point >= B.begin_point and B.data_item = 'THROUGH_LANES'"))

    data.formatted <- data.table(
    sqldf("select A.*, B.value_numeric as URBAN_CODE 
           from [data.formatted] A 
           left join [data.formatted] B on A.route_id = B.route_id and 
                                           A.year_record = B.year_record and 
                                           A.state_code = B.state_code and 
                                           A.begin_point <= B.end_point and 
                                           A.begin_point >= B.begin_point and 
                                           A.end_point <= B.end_point and 
                                           A.end_point >= B.begin_point and B.data_item = 'URBAN_CODE'"))
        
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
  data.formatted[, F_SYSTEM := c(NA,1,1,1,2,2,NA)[F_SYSTEM]]
  
  # remove non-inventory data
  data.formatted <- data.formatted[FACILITY_TYPE!=6,]
  
  # merge in expansion factors
  #sp <- data.table(read.table(spfile,sep="|",header=TRUE,stringsAsFactors=FALSE))

  con <- odbcConnect("HPMS")

  sp <- sqlQuery(con,paste0("select * from samples",year," where state_code = ",getStateNumFromCode(state)," and year_record = ",year))
  
  odbcClose(con)
  
  sp <- data.table(sp)
  
  
  setnames(sp,
          c("YEAR_RECORD", "STATE_CODE", "ROUTE_ID","BEGIN_POINT", "END_POINT", "SAMPLE_ID","EXPANSION_FACTOR"),
          c("year_record", "state_code", "route_id","begin_point", "end_point", "sample_id","expansion_factor"))
  
  data.formatted <- data.table(
    sqldf("select A.*, B.expansion_factor as expansion_factor
           from [data.formatted] A 
           left join [sp] B on A.route_id = B.route_id and 
                                           A.year_record = B.year_record and 
                                           A.state_code = B.state_code and 
                                           A.begin_point <= B.end_point and 
                                           A.begin_point >= B.begin_point and 
                                           A.end_point <= B.end_point and 
                                           A.end_point >= B.begin_point"))
  
  data.formatted[,expansion_factor:=as.numeric(expansion_factor)]
  
  #data.formatted[, expansion_factor := NA]
  #data <- data.formatted
  
  setkeyv(data.formatted,c("year_record","route_id","data_item"))
  
  gc()
  
  return(data.formatted)
  
}

# Save a data set in the appropriate location for a given state and year
SaveDataSet <- function(year, state, dat) {
  
  state <- getStateLabelFromNum(state)
  
  path <- paste0("data/", state)
  file <- paste0(year, ".RDS")
  fullpath <- paste0(path, "/", file)
  
  # Create new directory if needed
  if (!dir.exists(path)) dir.create(path)
  
  # See if a data set for this year and state already exists
  okayToSave <- TRUE
  if (file %in% dir(path)) {
    # File exists, check with user
    if(!(goverwrite %in% c("ALL Y","ALL N")))
    {
      overwrite <- getUserInput(valid = c("Y", "y", "N", "n","ALL Y","ALL N"), prompt = paste0("Processed file already exists for ", state, " ", year, ". Overwrite it? (Y/N/ALL Y/ALL N): "))
    } 
    if(goverwrite == "ALL Y")
    {
      overwrite <- "Y"
    }
    if(goverwrite == "ALL N")
    {
      overwrite <- "N"
    }
    if(overwrite=="ALL Y")
    {
      goverwrite <<- overwrite
      overwrite <- "Y"
    } 
    if(overwrite=="ALL N")
    {
      goverwrite <<- overwrite
      overwrite <- "N"
    }  
      
    if (!tolower(overwrite) == "y") okayToSave <- FALSE
  }
  
  if (okayToSave) saveRDS(dat, file = fullpath)
  
}
