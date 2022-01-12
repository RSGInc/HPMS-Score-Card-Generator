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

  query <- paste("select distinct StateId AS State_CodeYearRecord AS Year_Record,  from", sections_table,
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
    
    year  <- getUserInput(prompt="\nWhat year(s) would you like to import?\nEnter a single year or a range as in 2017-2019.\nFor all years, type ALL.")
    
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
  
  if ( debugmode ) browser()
  
  for (state in states) {
    
    for(year in years) {
      
      # Create filename to save to

      state_name <- getStateLabel(state)
      state_code <- getStateNumFromCode(state)
      
      path <- file.path("data", state_name)
      file <- paste0(year, ".rds")
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
        
        if(overwrite == "ALL Y"){
          goverwrite <<- overwrite
          overwrite <- "Y"
        } 
        
        if(overwrite == "ALL N"){
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
        
        tryCatch({
          data <- FormatDataSet(dat=data, state_abbr=state, year=year)
          },
          error=function(cond) {
            message(cond)
            data <- NULL
          })
        
        cat(" complete!\n")
        
        if ( is.null(data) | nrow(data) == 0 ){
          success <- c(success, FALSE)
          
         } else {
          
           # Save data sets ------------------
          
          cat("Saving to", fullpath, '...')
          
          # Create new directory if needed
          if (!dir.exists(path)) dir.create(path, recursive = TRUE)
          
          saveRDS(data, file = fullpath)
          
          cat(" complete!\n")
          success <- c(success, TRUE)
          
        }
        
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

  query <- paste0('select YearRecord AS Year_Record, StateId AS State_Code, RouteID AS Route_ID, BeginPoint AS Begin_Point, EndPoint AS End_Point, DataItem AS Data_Item, SectionLength AS Section_Length, ValueNumeric AS Value_Numeric, ValueText AS Value_Text, ValueDate AS Value_Date, StateYearKey from ', sections_table, ' where StateYearKey = ',
                  getStateNumFromCode(state), as.numeric(year) %% 100)
  
  data <- sqlQuery(con, query, stringsAsFactors=FALSE)
  
  if ( nrow(data) == 0 ){
    stop('Query returned zero rows')
  }
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

append_column = function(data,column){
  
  data.column = data[data_item==column,.(year_record,route_id,begin_point,end_point,value_numeric)]
  setnames(data.column,"value_numeric",column)
  data[data.column,(column):=get(column),on=.(year_record,route_id,begin_point,end_point)]
  return(data)
}

# Format a given data set for use in the score card generation process
FormatDataSet <- function(dat, state_abbr, year) {

  # summary of the sections that are dropped as part of this formatting
  # 1. Any section where FACILITY_TYPE = 6 or 7. 6 is Non-Inventory Direction and 7 is Planned/Unbuilt. We had previously only been removing where facility_type = 6 so the exclusion of the Planned/Unbuilt is new.
  #	Should we also exclude 5 "Non Mainline"? The HPMS guide says "Public road mileage is based only on sections coded '1,' or '2'. This includes only those roads that are open to public travel regardless of the ownership or maintenance responsibilities. Ramps are not included in the public road mileage calculation."
  # Any section that is SP or SP* and does not have a corresponding expansion factor.
  # Any section that is facility_type = 4 (a ramp) but has an extent that is not "FE + R". We should check with Justin if ramps should be included with SP data items. I don't think they should be.
  # Any section that has a blank section extent in the extent details spreadsheet.
  
  # Keep track of the original sections 
  dat <- dat[order(route_id, data_item, begin_point)]
  dat[, section_id := 1:.N, by=.(route_id, data_item)]
  dat[, c('begin_point_og', 'end_point_og') := list(begin_point, end_point)]
  
  data.formatted = expand(dat,0.1)
  
  # Merge data on itself to convert rows to columns
  data.formatted = append_column(data.formatted,"F_SYSTEM")
  data.formatted = append_column(data.formatted,"NHS")
  data.formatted = append_column(data.formatted,"FACILITY_TYPE")
  data.formatted = append_column(data.formatted,"THROUGH_LANES")
  data.formatted = append_column(data.formatted,"URBAN_CODE")

  # THROUGH_LANES needs to be smarter
  # Lane Miles - 
  # (Through_Lanes x F_System in (1,2,3,4,5,6-Urban)) plus
  # (total length for Rural Minor Collectors x 2)
  
  data.formatted[F_SYSTEM >= 6 & URBAN_CODE == 99999 & FACILITY_TYPE < 6 &
                   is.na(THROUGH_LANES), THROUGH_LANES:=2]
  
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
  data_noFT6 <- data.formatted[!FACILITY_TYPE %in% c(5,6,7), ] # keeping 1,2,4. 3 is not a code in the field guide
  
  # keeping only ramps for the ramp detail data items
  # ramp detail data items are "AADT","F_SYSTEM","FACILITY_TYPE","THROUGH_LANES","URBAN_CODE"
  
  data_noFT6 = data_noFT6[(FACILITY_TYPE == 4 & 
                             data_item %in% c("AADT","F_SYSTEM","FACILITY_TYPE","THROUGH_LANES","URBAN_CODE"))|
                            (FACILITY_TYPE %in% c(1,2))]
  
  # merge in expansion factors ---------------------------------------------

  con <- odbcConnect("HPMS")

  query <- paste0('select 
    YearRecord AS Year_Record, 
    StateId AS State_Code, 
    RouteID AS Route_ID, 
    BeginPoint AS Begin_Point, 
    EndPoint AS End_Point, 
    SectionLength AS Section_Length, 
    SampleID AS Sample_ID, 
    ExpansionFactor AS Expansion_Factor,
    Comments,
    LastModifiedOn AS Last_Modified_On,
    LastModifiedBy AS Last_Modified_By,
    Invalid, 
    StateYearKey 
    from ', samples_table, ' where StateYearKey = ',
                  getStateNumFromCode(state_abbr), as.numeric(year) %% 100)
 
  sp <- sqlQuery(con, query,stringsAsFactors=FALSE)
  
  odbcClose(con)
  
  if (nrow(sp) > 0){
    
    sp <- cleanUpQuery(sp)
    
    stopifnot(sp[str_detect(route_id, 'e[+-][0-9]'), .N] == 0,
              data_noFT6[str_detect(route_id, 'e[+-][0-9]'), .N] == 0)
    
    sp = expand(sp,0.1)
    
    # things we do not need in SP
    
    sp[,num_sections:=NULL]
    sp[,section_length:=NULL]
    sp[,stateyearkey:=NULL]
    sp[,state_code:=NULL]
    
    data_noFT6[, route_id := as.character(route_id)]
    sp[, route_id := as.character(route_id)]
    
    
    # Checks ----------------------------------------------
    rid_sec = data_noFT6[, route_id] %>% unique() %>% sort()
    rid_sp = sp[, route_id] %>% unique() %>% sort()

    # route_ID in samples but not in sections? (should be zero)
    if ( 
      length(setdiff(rid_sp, rid_sec)) > 0 |
      length(intersect(rid_sp, rid_sec)) < length(rid_sp)
    ){ 
      
      browser() 
      
      compare_ids = data.table(sp = rid_sp, sec = rid_sec[1:length(rid_sp)])
      
      # Check for data in sp that don't match anything in data_noFT6
      join_vars = c('year_record', 'route_id', 'begin_point', 'end_point')
      
      problems1 = data_noFT6[!sp, on=join_vars]
      problems2 = sp[!data_noFT6, on=join_vars]
      spmatch = data_noFT6[sp, on=join_vars, nomatch=NULL]
    }
    
    data_exp = merge(data_noFT6, sp,
                     by = c('year_record', 'route_id', 'begin_point', 'end_point'),
                     all.x=TRUE)
    
    data_exp[, expansion_factor := as.numeric(expansion_factor)]
    
  } else {
    data_exp <- data_noFT6
    data_exp[, expansion_factor := as.numeric(NA)]
    warning('Result of query:"', query, '" had zero rows.')
  }
  
  rm(data.formatted)
  
  data_exp[, rural_urban := c("Urban","Rural")[1 + 1 * (URBAN_CODE == 99999)]]
  
  data_exp = merge(
    data_exp,
    gExtentDetail,
    by  = c('data_item', 'rural_urban'),
    all.x=TRUE)

  # Create section_extent based on data item, f-system and rural/urban designation
  data_exp[, section_extent := ""]
  
  data_exp[NHS == 1, section_extent := nhs]
  data_exp[section_extent == "" & F_SYTEMorig==1, section_extent := fs1]
  data_exp[section_extent == "" & F_SYTEMorig==2, section_extent := fs2]
  data_exp[section_extent == "" & F_SYTEMorig==3, section_extent := fs3]
  data_exp[section_extent == "" & F_SYTEMorig==4, section_extent := fs4]
  data_exp[section_extent == "" & F_SYTEMorig==5, section_extent := fs5]
  data_exp[section_extent == "" & F_SYTEMorig==6, section_extent := fs6]
  data_exp[section_extent == "" & F_SYTEMorig==7, section_extent := fs7]
  
    
  # Filter out sections that are not needed.  E.g. SP items with no expansion factor
  # or sample_id
  # E.g. Ramps (FACILITY_TYPE = 4) for non FE+R
  
  warning(
    "Mileage removed for SP sections with no expansion factors: ",
    data_exp[
      (section_extent %in% c('SP', 'SP*') & is.na(data_exp$expansion_factor)),
      sum(end_point-begin_point)],
    "\n")
  
  data_exp = data_exp[
    !(section_extent %in% c('SP', 'SP*') & is.na(data_exp$expansion_factor))]
  
  
  if ( debugmode ){
    
    browser()
    
    
    data_exp[, has_se := 1 * (section_extent != '')]
    
    data_exp[, has_ext := 1 * !is.na(extent)]
    
    data_exp[, .N, has_ext]
    
    data_exp[, .N, keyby=.(has_se, data_item, rural_urban)] %>%
      dcast(data_item + rural_urban ~ has_se, value.var = 'N')
  
    data_exp[, .N, keyby=.(has_se, rural_urban, F_SYTEMorig)] %>%
      dcast(rural_urban + F_SYTEMorig ~ has_se, value.var = 'N')
    
    data_exp[, .N, keyby=.(has_se, rural_urban, F_SYTEMorig)] %>%
      dcast(rural_urban + F_SYTEMorig ~ has_se, value.var = 'N')
    
    data_exp[, .N, keyby= .(has_se, has_ef = 1 * !is.na(expansion_factor))]
  }
  
  
  # Drop rows that have no designated section_extent
  # FIXME: why would these have no section_extent?
  # depends on F_SYSTEM, rural/urban, and data item
  
  data_exp = data_exp[section_extent != '',]
  

  # Prepare to write out the data
  
  drop_cols  = c(
    "extent", "rural_urban", "nhs",
    "fs1", "fs2", "fs3", "fs4", "fs5", "fs6", "fs7")
  
  data_exp[, (drop_cols) := NULL]
  
  setkeyv(data_exp, c("state_code","year_record","route_id","data_item","begin_point","end_point"))

  # if ( nrow(data_exp) == 0 ){browser()}
  return(data_exp)
  
}

