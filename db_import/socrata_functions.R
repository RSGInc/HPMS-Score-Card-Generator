

create_sections_tables = function(
  events_url,
  designation_url,
  overwrite = FALSE
) {
  
  # TODO: put into make_cache_path
  ## Make path -----------------------------------------------------------------
  # Get state abbreviation, ensure both urls are for same state
  state_suffix = substr( names(events_url), 
                         nchar( names(events_url) ) - 3 + 1, 
                         nchar( names(events_url) ))
  
  state_check = substr( names(designation_url), 
                        nchar( names(designation_url) ) - 3 + 1, 
                        nchar( names(designation_url) ))
  
  if( state_suffix != state_check ) browser()
  
  temp_url = c( sections = 'dummy_var')
  names(temp_url) = paste0( names(temp_url), state_suffix)
  # ----------------------------------------------------------------------------
  
  cache_path = make_cache_path( temp_url )
  
  events_path      = download_socrata(events_url, overwrite = overwrite)
  designation_path = download_socrata(designation_url, overwrite = overwrite)
  
  events_tbl      = readRDS(events_path)
  designation_tbl = readRDS(designation_path)

  if ( !'valuedate' %in% tolower(names(designation_tbl)) ) {

    designation_tbl[, valuedate := NA_character_ ]

  }
  
  # can assume the data is okay from here, skip to rbind -----------------------
  # Check fields now match
  events_only = setdiff(names(events_tbl), names(designation_tbl))
  desigs_only = setdiff(names(designation_tbl), names(events_tbl))
  
  if ( length(events_only) > 0 |
       length(desigs_only) > 0 ){
    
    browser()
    
  }
  
  # Coerce into characters -- read.socrata will import dates as POSIXct
  cols = names(events_tbl)
  events_tbl[, (cols) := lapply(.SD, as.character), .SDcols = cols]
  designation_tbl[, (cols) := lapply(.SD, as.character), .SDcols = cols]
  
  
  # Bind to create final table
  setcolorder(designation_tbl, neworder = names(events_tbl))
  sections_tbl = rbind(events_tbl, designation_tbl)
  #-----------------------------------------------------------------------------
  message('...saving to ', cache_path)
  
  # The data come in with many "NULL" entries, but they are literally interpreted as
  # a string reading "NULL" instead of NA's
  #
  # Replace 'NULL' with NA's 
  lapply( names(sections_tbl), function(x) { sections_tbl[get(x) == "NULL", (x) := NA_character_] } )

  saveRDS(sections_tbl, cache_path)

  return(cache_path)
  
}

read.socrata2 = function (
  url, 
  app_token = NULL, 
  email = NULL, 
  password = NULL, 
  stringsAsFactors = FALSE
) {

  validUrl <- validateUrl(url, app_token)
  parsedUrl <- httr::parse_url(validUrl)
  mimeType <- mime::guess_type(parsedUrl$path)
  
  # browser()
  
  if (!is.null(names(parsedUrl$query)) & all(names(parsedUrl$query) != '$query')) {
    orderTest <- any(names(parsedUrl$query) == "$order")
    queries <- unlist(parsedUrl$query)
    countTest <- any(startsWith(queries, "count"))
    if (!orderTest & !countTest){
      validUrl <- paste(validUrl, "?$order=:id", sep = "")
      parsedUrl <- httr::parse_url(validUrl)
    } 
  }

  if (!(mimeType %in% c("text/csv", "application/json"))) 
    stop("Error in read.socrata: ", mimeType, " not a supported data format.")

  response <- RSocrata:::getResponse(validUrl, email, password)
  page <- RSocrata:::getContentAsDataFrame(response)
  result <- page
  
  return(result)
}

make_cache_path = function(url, dir='db_import/download_cache'){
  dir.create(dir, showWarnings=FALSE)
  
  fname = names(url)
  
  if ( fname == '' ){
    fname = url %>%
      basename() %>%
      str_replace('[.].*$', '')
  }
  
  path = file.path(dir, paste0(fname, '.rds'))
  return(path)
  
}

download_socrata = function(url, overwrite=FALSE){

  cache_path = make_cache_path(url)  

  if ( !file.exists(cache_path) | overwrite ){
    
    if (!str_detect(url, 'json$|csv$')){
      stop('Only .json based URLs please')
    }
    
    # Get counts for checking 

    query = paste0(
      url,
      '?$query=SELECT StateId,DataYear,count(StateId) ',
      'GROUP BY StateId, DataYear ',
      'ORDER BY StateId, DataYear'
    )
    
    counts_remote = read.socrata2(query, email=email, password=password)
    names(counts_remote) = str_to_lower(names(counts_remote))
    setnames(counts_remote, 'count_stateid', 'n_remote')
    
    message('Downloading ', sum(as.numeric(counts_remote[, 'n_remote'])), ' rows from ', names(url))
    
    dt = read.socrata(url=url, email=email, password=password)
    setDT(dt)
    names(dt) = str_to_lower(names(dt))
    
    counts_local = dt[, .(n_local = .N), by=.(stateid, datayear)]
    
    counts_check = merge(counts_remote, counts_local,
                         by = c('stateid', 'datayear'), all=TRUE)
    setDT(counts_check)
    
    stopifnot(counts_check[n_local != n_remote, .N] == 0)
    
    
    message('Downloaded...saving to ', cache_path)
    
    saveRDS(dt, cache_path)
  }
  
  return(cache_path)
  
}

write_to_stage = function(cache_path, con, stage_table, chunk_size=100000){
  
  message('Reading from ', cache_path)
  
  dt = readRDS(cache_path)
  # if ( datayear == '2022' ) use these below
  col_type_chk = c(
    datayear        = 'integer',
    stateid         = 'integer',
    routeid         = 'character',
    sampleid        = 'character',
    beginpoint      = 'numeric',
    endpoint        = 'numeric',
    expansionfactor = 'numeric',
    comments        = 'character',
    dataitem        = 'character',
    valuenumeric    = 'numeric',
    valuetext       = 'character',
    begindate       = 'POSIXct'
  )
  coltype_chk_dt = data.table(field = names(col_type_chk), chk = col_type_chk)
  
  message('Updating data types')
  
  if ( class(dt$valuedate)[1] == 'character' ){
    if ( str_length(dt[!is.na(valuedate), valuedate][1]) > 10 ){
      dt[, valuedate := ymd_hms(valuedate)]
      # dt[, valuedate := as.POSIXct(ymd_hms(valuedate))]
    } else {
      coltype_chk_dt[field == 'valuedate', chk := 'Date']
      dt[, valuedate := ymd(valuedate)]
      # dt[, valuedate := as.POSIXct(ymd(valuedate))]
    }
  }
  
  # dt[, datayear := as.integer(datayear)]
  # dt[, stateid := as.integer(stateid)]
  # dt[, beginpoint := as.numeric(beginpoint)]
  # dt[, endpoint := as.numeric(endpoint)]
  dt[, datayear   := as.integer(datayear)]
  dt[, stateid    := as.integer(stateid)]
  dt[, beginpoint := as.numeric(beginpoint)]
  dt[, endpoint   := as.numeric(endpoint)]
  
  
  if ( 'valuenumeric' %in% names(dt) ){
    dt[, valuenumeric := as.numeric(valuenumeric)]
  }
  
  if( 'expansionfactor' %in% names(dt) ){
    
    dt[, expansionfactor := as.numeric(expansionfactor) ]
    
  }
  
  if( 'begindate' %in% names(dt) ){
    
    dt[, begindate := as.POSIXct(begindate)]
    
  }
  
  message('Checking data types')
  col_type_obs = sapply(dt, function(x) class(x)[1])
  coltype_obs_dt = data.table(field = names(col_type_obs), obs = col_type_obs)
  
  coltype_dt = merge(coltype_chk_dt, coltype_obs_dt, by='field')
  
  if ( coltype_dt[chk != obs, .N] > 0 ){
    
    message('Column type mismatch')
    print(coltype_dt[chk != obs])
    browser()
    
  }
  
  message('Dropping old values from stage table')
  
  states = unique(dt[, stateid])
  years = unique(dt[, datayear])
  
  if ( stage_table %in% dbListTables(conn = con) ){
    sql = paste0(
      'DELETE FROM ', stage_table,
      ' WHERE stateid in (', paste(states, collapse=', '), ')',
      ' AND datayear in (', paste(years, collapse=', '), ')')
    dbExecute(con, sql)
    
    # Compare field names
    
    old_fields = dbListFields(con, stage_table)
    old_fields = str_to_lower(old_fields)
    
    if ( stage_table == 'rs_stage' ){
      old_fields = setdiff(old_fields, 'stateyearkey') # this is computed in rs_stage
    }
    
    new_fields = names(dt)
    
    # Create StateYearKey
    if ( !'stateyearkey' %in% new_fields & stage_table == 'rss_stage'){
      dt[, stateyearkey := paste0(stateid, str_sub(datayear, start=3, end=4))]
      new_fields = c(new_fields, 'stateyearkey')
    }

    if ( !('valuetext' %in% new_fields) & stage_table == 'rs_stage' ){
      dt[, valuetext := NA_character_]
      new_fields = c(new_fields, 'valuetext')
    }    
    
    if ( length(setdiff(old_fields, new_fields)) > 0 |
         length(setdiff(new_fields, old_fields)) > 0 ){
      browser()
      only_old = setdiff(old_fields, new_fields)
      only_new = setdiff(new_fields, old_fields)
      
    }
   
    dt = dt[, intersect(old_fields, new_fields), with=FALSE]
    
  }
  
  # Write in chunks

  idx_seq = seq(from=1, to = dt[, .N], by = chunk_size)
  
  for ( i in seq_len(length(idx_seq)) ){
    start = idx_seq[i]
    if ( i < length(idx_seq) ){
      stop = (idx_seq[i + 1] - 1)
    } else {
      stop = dt[, .N]
    }
    
    chunk = dt[start:stop] 
    message('Writing chunk ', i, '/', length(idx_seq), ' with ', chunk[, .N], ' rows')

    dbWriteTable(con, stage_table, chunk, append=TRUE)
  
  }
  
  message('Done!')
  
  
  # Check that we have the right number of rows for each state.
  counts_cache = dt[, .(n_cache = .N), by=.(stateid, datayear)]
  
  dt_stage = tbl(con, from=stage_table)
  
  counts_stage = dt_stage %>%
    count(stateid, datayear) %>%
    collect() %>%
    rename(n_stage = n)
  
  counts_check = merge(counts_cache, counts_stage,
                       by = c('stateid', 'datayear'), all=TRUE)
  setDT(counts_check)
  
  stopifnot(counts_check[n_cache != n_stage, .N] == 0)
  
  return(counts_stage)
  
}

copy_rows = function(con, stage_table, prod_table, counts_local){
  # Move data from stage to production 
  
  # Compare counts ----------------------------------------------------------
  
  # years = unique(counts_stage$datayear)
  # states = unique(counts_stage$stateid)
  stage = tbl(con, from=stage_table)
  
  counts_stage = stage %>%
    count(datayear, stateid) %>%
    collect() %>%
    rename(n_stage = n)
  
  years = unique(counts_stage$datayear)
  states = unique(counts_stage$stateid)
  
  # Compare fields -------------------------------------------------------
  
  # prod = tbl(con, from=prod_table)
  
  old_fields = dbListFields(con, prod_table)
  new_fields = dbListFields(con, stage_table)
  
  # Create StateYearKey
  if ( !'stateyearkey' %in% str_to_lower(new_fields) ){
    sql = paste0('alter table ', stage_table,
                 ' add StateYearKey as (stateid * 100 + datayear % 1000)')
    dbExecute(con, sql)
    new_fields = c(new_fields, 'StateYearKey')
  }
  
  # Create sectionlength
  if ( !'sectionlength' %in% str_to_lower(new_fields) ){
    sql = str_glue(
      'alter table {stage_table} ',
      'add SectionLength as (EndPoint - BeginPoint)')
    dbExecute(con, sql)
    new_fields = c(new_fields, 'SectionLength')
  }
  
  # setdiff(tolower(new_fields), tolower(old_fields))
  new_fields = new_fields[!new_fields %in% 'natrouteid']
  
  if (
    length(setdiff(tolower(new_fields), tolower(old_fields))) > 0 |
    length(setdiff(tolower(old_fields), tolower(new_fields))) > 0
  ){
    browser()
  }
  
  # Copy rows into production table.
  message('Dropping rows from production')
  
  sql = paste0(
    'DELETE FROM ', prod_table,
    ' WHERE stateid in (', paste(states, collapse=', '), ')',
    ' AND datayear in (', paste(years, collapse=', '), ')')
  dbExecute(con, sql)
  
  message('Copying from stage to production')
  sql = paste0('insert into ', prod_table, '(', paste(new_fields, collapse=', '),
               ') select ', paste(new_fields, collapse=', '), ' from ', stage_table)
  dbExecute(con, sql)
  message('Done')
  
  # Get counts in production table
  prod = tbl(con, from = prod_table)
  counts_prod = prod %>%
    filter(DataYear %in% years,
           StateId %in% states) %>%
    count(StateId, DataYear) %>%
    collect() %>%
    rename(stateid = StateId, datayear = DataYear, n_prod = n)
  
  counts_check = merge(counts_stage, counts_prod, by = c('stateid', 'datayear'))
  setDT(counts_check)
  
  stopifnot(counts_check[n_stage != n_prod, .N] == 0)
  
  sql = paste0(
    'DELETE FROM ', stage_table,
    ' WHERE stateid in (', paste(states, collapse=', '), ')',
    ' AND datayear in (', paste(years, collapse=', '), ')')
  dbExecute(con, sql)
  
}
