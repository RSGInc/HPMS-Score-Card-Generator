





format_report <- function(report, highlight_threshold){

  report[, miles := string_format(miles)]
  report[, N := string_format(N)]
  
  if(nrow(report) > 0){
    
    #report <- merge(data.table(groupCat=1:4),report,by="groupCat",all.x=T)
    report[, groupCat := as.character(groupCat)]
    report[, Name := as.character(Name)]
    
    # Test if columns are all the same type to prevent warnings
    
    if ( report %>%
         sapply(class) %>%
         unique() %>%
         length() > 1 ){
      report = report %>%
        lapply(as.character) %>%
        as.data.table()
    }
    
    report <- data.table(melt(report, id.vars="groupCat"))

    report[value == 'NA', value := NA]
    
    # Determine which rows to highlight
    report[, highlight := 0]
    report[variable == 'perc_miles' & as.numeric(str_replace_all(value, ',', '')) > highlight_threshold,
           highlight := 1]
    
    report[variable == "perc_miles" & !is.na(value), value := paste0(value,"%")]
    report[variable == "Name", value := gF_SYSTEM_levels[as.numeric(groupCat)],]
    report[variable == "Name", variable := "Functional\nSystem",]
    report[variable == "N", variable := "Number of\nSections",]
    report[variable == "miles", variable := "Total Centerline\nMiles",]
    report[variable == "perc_miles", variable := "% of All\n Miles\nSubmitted",]
    report[is.na(highlight), highlight := 0]
    return(report)

  } else {
    return(NULL)
  }

}
