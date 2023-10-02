


if(variable %like% 'CURVES|GRADES'){
 
  # Calculate fraction of rows with expansionfactors
  score = data[dataitem == variable & datayear == year & !is.na(expansionfactor), .N] /
    data[dataitem == variable & datayear == year, .N]
  
  return(score)
  
} 


if( variable %like% 'GRADE' ){
  
  # sum on all must equal to sample length on .... what is 'sample length?
  dat.grades_all = data[dataitem %like% 'GRADES' & datayear == year,] # !is.na(expansionfactor)  #?
  dat.grades_all[, sample := !is.na(expansionfactor)]
  
  sum_all_grades = dat.grades_all[, .(sum(sectionlength)), by = sampleid]  # include na.rm?
  
  # sample length criteria
  dat.sample = 
    dat.grades_all[ ( sample == TRUE & F_SYSTEM %in% c(1,2,3) ) |
                    ( F_SYSTEM == 4 & URBAN_ID == 99999 ) , ]
  
  sum( sum_all_grades$V1 )
  sum( dat.sample$sectionlength )
  
  score2 = sum( dat.sample$sectionlength )/sum( sum_all_grades$V1 )
  
  stopifnot(
    
    sum( sum_all_grades$V1 ) == sum(dat.sample$sectionlength)
    
  )
}
  
# TODO: how to score with multiple criteria? equal weighting?
if( variable %like% 'CURVES' ){
  
  dat.variable   = data[dataitem == variable & datayear == year,]
  dat.curves_all = data[dataitem %like% 'CURVES' & datayear == year, ]# Needed for new checks in HPMS9
  
  ##
  
  all_samples = unique(data$sampleid)
  samples_cross_curves = 
    data[ dataitem %like% 'CURVES' & datayear == year &
         ( F_SYSTEM %in% c(1, 2, 3) | ( F_SYSTEM == 4 & URBAN_ID == 99999) ), 
    ]
  
  dat.SURFACE_TYPE = data[dataitem == "SURFACE_TYPE" & datayear == year,]
  
  
  # number of samples where curve A-F is coded
  n_curves_coded = sum( 1 * ( all_samples %in% samples_cross_curves$sampleid ) )
  n_second_calc  = length( unique(samples_cross_curves$sampleid) )
  
  score_y = n_curves_coded/length( all_samples )
  # Sum length on all curves = Sample length on ( 1 or 2 )
  dat.curves_all[, .( sum(sectionlength, na.rm = TRUE) )]
}

### Reference
coverage2 <- sqldf(
  "select 
        A.routeid,A.beginpoint,A.endpoint,A.dataitem,A.valuenumeric as SURFACE_TYPE, 
        B.valuenumeric as variable,B.expansionfactor, B.F_SYSTEM, B.URBAN_ID, B.dataitem as dataitem_b
        from [dat.SURFACE_TYPE] A 
        left join [dat.variable] B on 
        A.routeid = B.routeid and (
        ( A.beginpoint between B.beginpoint and B.endpoint and A.endpoint between B.beginpoint and B.endpoint ) or
        ( B.beginpoint between A.beginpoint and A.endpoint and B.endpoint between A.beginpoint and A.endpoint )
        )")

setDT(coverage2)

coverage <- sqldf(
  "select 
        A.routeid,A.beginpoint,A.endpoint,A.dataitem,A.valuenumeric as SURFACE_TYPE, 
        B.valuenumeric as variable,B.expansionfactor 
        from [dat.SURFACE_TYPE] A 
        left join [dat.variable] B on 
        A.routeid = B.routeid and (
        ( A.beginpoint between B.beginpoint and B.endpoint and A.endpoint between B.beginpoint and B.endpoint ) or
        ( B.beginpoint between A.beginpoint and A.endpoint and B.endpoint between A.beginpoint and A.endpoint )
        )")

setDT(coverage)

coverage[, required := !is.na(expansionfactor) & SURFACE_TYPE %in% c(3,4,9,10)]