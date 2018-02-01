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
  # Urban_Code <99999 and Facility_Type is in (1,2))
  #
  # Lane Miles - 
  # Same as above but,  (Through_Lanes x F_System in (1,2,3,4,5)) plus
  # (total length for Rural Minor Collectors x 2)
  

  n_Records.1   <- data[state_code == state & year_record == year, .N, ]        
  n_Records.2   <- data[state_code == state & year_record == year_compare, .N, ]

  # Generate an index to keep only certain rows that meet certain criteria
  idx_st_fsystem <- with(data, (state_code == state & data_item == 'F_SYSTEM'))
  idx_fs15 <- with(data, ((F_SYTEMorig %in% 1:5) & (FACILITY_TYPE %in% 1:2)) )
  idx_fs6 <- with(data, ((F_SYTEMorig == 6) & (URBAN_CODE < 99999) & (FACILITY_TYPE %in% c(1, 2))) )
  IDX <- idx_st_fsystem & (idx_fs15 | idx_fs6)

  # Centerline
  n_CtrLine.1 <- data[IDX & year_record == year,         sum(end_point-begin_point,na.rm = TRUE),]
  n_CtrLine.2 <- data[IDX & year_record == year_compare, sum(end_point-begin_point,na.rm = TRUE),]

  # Lane miles
  n_LaneMiles.1 <- data[IDX & year_record==year,
                        sum(THROUGH_LANES * (end_point - begin_point), na.rm = TRUE), ] + 
    data[idx_st_fsystem & year_record == year & (F_SYTEMorig == 6) & (URBAN_CODE == 99999),
         sum(2 * (end_point - begin_point), na.rm = TRUE), ]
  
  n_LaneMiles.2 <- data[IDX & year_record == year_compare,
                        sum(THROUGH_LANES * (end_point - begin_point), na.rm = TRUE), ] + 
    data[idx_st_fsystem & year_record == (year_compare) & (F_SYTEMorig == 6) & (URBAN_CODE == 99999),
         sum(2 * (end_point - begin_point), na.rm = TRUE), ]

  # Data items
  n_Variables.1 <- length(data[state_code == state & year_record == year, unique(data_item), ])
  n_Variables.2 <- length(data[state_code == state & year_record == year_compare, unique(data_item), ])

  
  # Route ids
  route_id.1 <- unique(data[IDX & year_record == year]$route_id)
  route_id.2 <- unique(data[IDX & year_record == year_compare]$route_id)

  n_routes.1 <- length(route_id.1)
  n_routes.2 <- length(route_id.2)

  # Route ids with no match in the other year
  n_route_nomatch.1 <- sum(!route_id.1 %in% route_id.2)
  n_route_nomatch.2 <- sum(!route_id.2 %in% route_id.1)

  pct_route_nomatch.1 <- n_route_nomatch.1 / n_routes.1 * 100
  pct_route_nomatch.2 <- n_route_nomatch.2 / n_routes.2 * 100
#browser()
  # Get n_sections  
  dt_sections.1 <- data[IDX & year_record == year,
                       list(route_id, begin_point, end_point)]
  
  dt_sections.2 <- data[IDX & year_record == year_compare,
                       list(route_id, begin_point, end_point)]

  # Add an id column so we can easily pull out the unmatched rows later
  dt_sections.1[, id_1 := 1:nrow(dt_sections.1)]
  dt_sections.2[, id_2 := 1:nrow(dt_sections.2)]
  
  n_sections.1 <- nrow(unique(dt_sections.1))
  n_sections.2 <- nrow(unique(dt_sections.2))

  # Match sections, then get counts of unmatched sections

  dt_sections_j <- sqldf(
    'select A.id_1, A.route_id, A.begin_point as [begin_point.1], A.end_point as [end_point.1],
            B.id_2, B.route_id, B.begin_point as [begin_point.2], B.end_point as [end_point.2]
     from [dt_sections.1] A
     left join [dt_sections.2] B on
     A.route_id = B.route_id and
     ((
        (A.begin_point >= B.begin_point) and
        (A.end_point <= B.end_point) and
        (A.begin_point <= B.end_point) and
        (A.end_point >= B.begin_point)
       ) or (
        (B.begin_point >= A.begin_point) and
        (B.end_point <= A.end_point) and
        (B.begin_point <= A.end_point) and
        (B.end_point >= A.begin_point)
       ))'
    )

  dt_sections_j <- data.table(dt_sections_j)
  
  dt_sec_nomatch.1 <- dt_sections_j[is.na(begin_point.2)]
  dt_sec_nomatch.2 <- dt_sections.2[!id_2 %in% dt_sections_j$id_2]

  pct_sec_nomatch.1 <- nrow(dt_sec_nomatch.1) / n_sections.1 * 100
  pct_sec_nomatch.2 <- nrow(dt_sec_nomatch.2) / n_sections.2 * 100
  
  # Put the data together into a table.
  result <- data.table(
    Label=c("Number of Records", "Number of Data Items",
            "Number of Routes", "Pct. Unmatched Routes",
            "Number of Sections", "Pct. Unmatched Sections",
            "Total Center Line Miles*", "Total Lane Miles*"),
    Val.1=string_format(c(n_Records.1, n_Variables.1,
                          n_routes.1, pct_route_nomatch.1,
                          n_sections.1, pct_sec_nomatch.1,
                          n_CtrLine.1, n_LaneMiles.1)),
    Val.2=string_format(c(n_Records.2, n_Variables.2,
                          n_routes.2, pct_route_nomatch.2,
                          n_sections.2, pct_route_nomatch.2,
                          n_CtrLine.2, n_LaneMiles.2))#,
  )
  
  setnames(result,"Val.1",paste0("Year ",year))
  setnames(result,"Val.2",paste0("Year ",year_compare))
  setnames(result,"Label","")
  
  return(result)
  
}
