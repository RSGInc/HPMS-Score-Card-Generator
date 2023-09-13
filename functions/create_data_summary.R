###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2015
# Author: Jeff Dumont
#
#
# Description:
#
# This function produces a graphical object as an overall report for the
# first page.
#
###########################################################################

create_data_summary <- function(data, state, year, year_compare){
  
  # Description from Justin on how to count up the number of miles to show.
  # Centerline Miles = (Total length for F_System (sum) where
  # F_System is in (1,2,3,4,5) and Facility_Type is in (1,2)) plus 
  # (total length for F_System where F_System is in (6) and
  # URBAN_ID <99999 and Facility_Type is in (1,2))
  #
  # Lane Miles - 
  # Same as above but,  (Through_Lanes x F_System in (1,2,3,4,5)) plus
  # (total length for Rural Minor Collectors x 2)
  

  n_Records.1   <- data[stateid == state & datayear == year, sum(num_sections), ]        
  n_Records.2   <- data[stateid == state & datayear == year_compare, sum(num_sections), ]

  # Generate an index to keep only certain rows that meet certain criteria
  idx_st_fsystem <- with(data, (stateid == state & data_item == 'F_SYSTEM'))
  idx_fs15 <- with(data, ((F_SYTEMorig %in% 1:5) & (FACILITY_TYPE %in% 1:2)) )
  idx_fs6 <- with(data, ((F_SYTEMorig == 6) & (URBAN_ID < 99999) & (FACILITY_TYPE %in% c(1, 2))) )
  IDX <- idx_st_fsystem & (idx_fs15 | idx_fs6)

  # Centerline
  n_CtrLine.1 <- data[IDX & datayear == year,         sum(end_point-beginpoint,na.rm = TRUE),]
  n_CtrLine.2 <- data[IDX & datayear == year_compare, sum(end_point-beginpoint,na.rm = TRUE),]

  # Lane miles
  n_LaneMiles.1 <- data[IDX & datayear==year,
                        sum(THROUGH_LANES * (end_point - beginpoint), na.rm = TRUE), ]
  
  n_LaneMiles.2 <- data[IDX & datayear == year_compare,
                        sum(THROUGH_LANES * (end_point - beginpoint), na.rm = TRUE), ]

  # Data items
  n_Variables.1 <- length(data[stateid == state & datayear == year, unique(data_item), ])
  n_Variables.2 <- length(data[stateid == state & datayear == year_compare, unique(data_item), ])
  
  # Route ids
  routeid.1 <- unique(data[IDX & datayear == year]$routeid)
  routeid.2 <- unique(data[IDX & datayear == year_compare]$routeid)

  n_routes.1 <- length(routeid.1)
  n_routes.2 <- length(routeid.2)

  # Route ids with no match in the other year
  n_route_nomatch.1 <- sum(!routeid.1 %in% routeid.2)
  n_route_nomatch.2 <- sum(!routeid.2 %in% routeid.1)

  pct_route_nomatch.1 <- n_route_nomatch.1 / n_routes.1 * 100
  pct_route_nomatch.2 <- n_route_nomatch.2 / n_routes.2 * 100

  # Get n_sections  
  dt_sections.1 <- data[IDX & datayear == year,
                       list(routeid, beginpoint, end_point, num_sections)]
  
  dt_sections.2 <- data[IDX & datayear == year_compare,
                       list(routeid, beginpoint, end_point, num_sections)]

  # Add an id column so we can easily pull out the unmatched rows later
  dt_sections.1[, id_1 := 1:nrow(dt_sections.1)]
  dt_sections.2[, id_2 := 1:nrow(dt_sections.2)]
  
  n_sections.1 <- round(sum(dt_sections.1$num_sections))
  n_sections.2 <- round(sum(dt_sections.2$num_sections))

  # Match sections, then get counts of unmatched sections

  dt_sections_j = dt_sections.2[dt_sections.1,on=.(routeid,beginpoint,end_point)]
  
  dt_sec_nomatch.1 <- dt_sections_j[is.na(id_2)]
  dt_sec_nomatch.2 <- dt_sections.2[!id_2 %in% dt_sections_j$id_2]

  pct_sec_nomatch.1 <- round(sum(dt_sec_nomatch.1$i.num_sections)) / n_sections.1 * 100
  pct_sec_nomatch.2 <- round(sum(dt_sec_nomatch.2$num_sections)) / n_sections.2 * 100
  
  # Put the data together into a table.
  result <- data.table(
    Label=c("Number of Records", "Number of Data Items",
            "Number of Routes", "Pct. Unmatched Routes",
            "Number of Sections", "Pct. Unmatched Sections",
            "Total Centerline Miles*", "Total Lane Miles*"),
    Val.1=string_format(c(n_Records.1, n_Variables.1,
                          n_routes.1, pct_route_nomatch.1,
                          n_sections.1, pct_sec_nomatch.1,
                          n_CtrLine.1, n_LaneMiles.1)),
    Val.2=string_format(c(n_Records.2, n_Variables.2,
                          n_routes.2, pct_route_nomatch.2,
                          n_sections.2, pct_route_nomatch.2,
                          n_CtrLine.2, n_LaneMiles.2))#,
  )
  
  # Clean up table
  result = result[-c(5, 6)]
  
  result[Label == 'Pct. Unmatched Routes',
         `:=`(Val.1 = ifelse(as.numeric(Val.1) < 1 & as.numeric(Val.1) > 0, '< 1', Val.1),
              Val.2 = ifelse(as.numeric(Val.2) < 1 & as.numeric(Val.2) > 0, '< 1', Val.2))]
  
  result[Label %in% c('Number of Data Items', 'Number of Routes', 'Pct. Unmatched Routes'),
         `:=`(Val.1 = str_replace(Val.1, '[.][0-9]+', ''),
              Val.2 = str_replace(Val.2, '[.][0-9]+', ''))]
  
  result[Label == 'Pct. Unmatched Routes', 
         `:=`(Val.1 = paste0(Val.1, '%'),
              Val.2 = paste0(Val.2, '%'))]
  
  
  setnames(result,"Val.1",paste0("Year ",year))
  setnames(result,"Val.2",paste0("Year ",year_compare))
  setnames(result,"Label","")
  
  return(result)
  
}
