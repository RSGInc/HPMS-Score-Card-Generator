

read.socrata2 = function (url, app_token = NULL, email = NULL, password = NULL, 
                          stringsAsFactors = FALSE) {
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

make_cache_name = function(url, dir='db_import/download_cache'){
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

  cache_path = make_cache_name(url)  

  if ( !file.exists(cache_path) | overwrite ){
    
    if (!str_detect(url, 'json$|csv$')){
      stop('Only .json based URLs please')
    }
    
    query = paste0(
      url,
      '?$query=SELECT state_code,year_record,count(state_code) ',
      'GROUP BY state_code, year_record ',
      'ORDER BY state_code, year_record'
    )
    
    counts_remote = read.socrata2(query, email=email, password=password)
    setnames(counts_remote, 'count_state_code', 'n_remote')
    
    message('Downloading ', sum(as.numeric(counts_remote[, 'n_remote'])), ' rows from ', names(url))
    
    dt = read.socrata(url=url, email=email, password=password)
    setDT(dt)
    
    counts_local = dt[, .(n_local = .N), by=.(state_code, year_record)]
    
    counts_check = merge(counts_remote, counts_local,
                         by = c('state_code', 'year_record'), all=TRUE)
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
  
  # For checking column types
  col_type_chk = c(
    year_record = 'integer',
    state_code = 'integer',
    route_id = 'character',
    begin_point = 'numeric',
    end_point = 'numeric',
    data_item = 'character',
    value_numeric = 'numeric',
    value_text = 'character',
    value_date = 'POSIXct',
    natroute_id = 'character'
  )
  
  coltype_chk_dt = data.table(field = names(col_type_chk), chk = col_type_chk)
  
  message('Updating data types')
  
  #if ( names(url) %in% c('mid_atlantic', 'SC', 'AZ', 'TN') ){
  if (class(dt$value_date)[1] == 'character'){
    dt[, value_date := ymd_hms(value_date)]
  }
  
  dt[, year_record := as.integer(year_record)]
  dt[, state_code := as.integer(state_code)]
  dt[, begin_point := as.numeric(begin_point)]
  dt[, end_point := as.numeric(end_point)]
  
  if ( 'value_numeric' %in% names(dt) ){
    dt[, value_numeric := as.numeric(value_numeric)]
  }
  
  message('Checking data types')
  col_type_obs = sapply(dt, function(x) class(x)[1])
  coltype_obs_dt = data.table(field = names(col_type_obs), obs = col_type_obs)
  
  coltype_dt = merge(coltype_chk_dt, coltype_obs_dt, by='field')
  
  if ( coltype_dt[chk != obs, .N] > 0 ){
    
    message('Column type mismatch')
    print(coltype_dt)
    browser()
    
  }
  
  message('Dropping old values from stage table')
  
  states = unique(dt[, state_code])
  years = unique(dt[, year_record])
  
  if ( stage_table %in% dbListTables(conn = con) ){
    sql = paste0(
      'DELETE FROM ', stage_table,
      ' WHERE state_code in (', paste(states, collapse=', '), ')',
      ' AND year_record in (', paste(years, collapse=', '), ')')
    dbExecute(con, sql)
    
    # Compare field names
    
    old_fields = dbListFields(con, stage_table)
    old_fields = old_fields[!old_fields %in% c('natroute_id', 'StateYearKey', 'Section_Length')]
    
    new_fields = names(dt)
    
    if ( length(setdiff(old_fields, new_fields)) > 0 |
         length(setdiff(new_fields, old_fields)) > 0 ){
      browser()
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
  counts_cache = dt[, .(n_cache = .N), by=.(state_code, year_record)]
  
  dt_stage = tbl(con, from=stage_table)
  
  counts_stage = dt_stage %>%
    count(state_code, year_record) %>%
    collect() %>%
    rename(n_stage = n)
  
  counts_check = merge(counts_cache, counts_stage,
                       by = c('state_code', 'year_record'), all=TRUE)
  setDT(counts_check)
  
  stopifnot(counts_check[n_cache != n_stage, .N] == 0)
  
  return(counts_stage)
  
}

copy_rows = function(con, stage_table, prod_table, counts_local){
  # Move data from stage to production 
  
  # Compare counts ----------------------------------------------------------
  
  # years = unique(counts_stage$year_record)
  # states = unique(counts_stage$state_code)
  stage = tbl(con, from=stage_table)
  
  counts_stage = stage %>%
    count(year_record, state_code) %>%
    collect() %>%
    rename(n_stage = n)
  
  years = unique(counts_stage$year_record)
  states = unique(counts_stage$state_code)
  
  # Compare fields -------------------------------------------------------
  
  # prod = tbl(con, from=prod_table)
  
  old_fields = dbListFields(con, prod_table)
  new_fields = dbListFields(con, stage_table)
  
  # Create StateYearKey
  if ( !'stateyearkey' %in% str_to_lower(new_fields) ){
    sql = paste0('alter table ', stage_table,
                 ' add StateYearKey as (state_code * 100 + year_record % 1000)')
    dbExecute(con, sql)
    new_fields = c(new_fields, 'StateYearKey')
  }
  
  # Create section_length
  if ( !'section_length' %in% str_to_lower(new_fields) ){
    sql = str_glue(
      'alter table {stage_table} ',
      'add Section_Length as (End_Point - Begin_Point)')
    dbExecute(con, sql)
    new_fields = c(new_fields, 'Section_Length')
  }
  
  # setdiff(tolower(new_fields), tolower(old_fields))
  new_fields = new_fields[!new_fields %in% 'natroute_id']
  
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
    ' WHERE state_code in (', paste(states, collapse=', '), ')',
    ' AND year_record in (', paste(years, collapse=', '), ')')
  dbExecute(con, sql)
  
  message('Copying from stage to production')
  sql = paste0('insert into ', prod_table, '(', paste(new_fields, collapse=', '),
               ') select ', paste(new_fields, collapse=', '), ' from ', stage_table)
  dbExecute(con, sql)
  message('Done')
  
  # Get counts in production table
  prod = tbl(con, from = prod_table)
  counts_prod = prod %>%
    filter(Year_Record %in% years,
           State_Code %in% states) %>%
    count(State_Code, Year_Record) %>%
    collect() %>%
    rename(state_code = State_Code, year_record = Year_Record, n_prod = n)
  
  counts_check = merge(counts_stage, counts_prod, by = c('state_code', 'year_record'))
  setDT(counts_check)
  
  stopifnot(counts_check[n_stage != n_prod, .N] == 0)
  
  sql = paste0(
    'DELETE FROM ', stage_table,
    ' WHERE state_code in (', paste(states, collapse=', '), ')',
    ' AND year_record in (', paste(years, collapse=', '), ')')
  dbExecute(con, sql)
  
}
