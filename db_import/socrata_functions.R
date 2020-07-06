

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