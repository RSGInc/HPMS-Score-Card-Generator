###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: June 2017
# Author: Matthew Landis
#
#
# Description:
#
# Generates a summary of pavement characteristics 
# for the top of "pavement: detailed reveiw"
#
###########################################################################

create_pavement_summary <- function(data, state, year){

  #browser()
  
  # Subset the data
  dt <- data[stateid == state & 
             datayear == year &
             dataitem %in% c('THROUGH_LANES', 'SURFACE_TYPE'),]
  
  # Create "by" variable for summaries
  # warning("Jeff, please check the calculation of the group variable",
  #        immediate. = TRUE)
  
  dt[, group := 0]
  dt[Interstate == 1, group := 1]
  dt[NHS == 1, group := 2]
  dt[F_SYSTEM == 1&(NHS!=1|is.na(NHS)), group := 3]
  dt[F_SYSTEM == 2&(NHS!=1|is.na(NHS)), group := 4]
  setkey(dt, group)
  
  # Check
  #dt[, list(dataitem, F_SYSTEM, Interstate, NHS, group)]
  #with(dt, table(group, F_SYSTEM, NHS, Interstate, useNA='always'))
  
  
  # Missing through lanes
  dt_through_lanes <- 
    dt[dataitem == "THROUGH_LANES",
       list(n_missing = sum(num_sections * ( is.na(valuenumeric) | is.null(valuenumeric)))),
       by=group]
  
  dt_through_lanes = merge(data.table(group=1:length(gF_SYSTEM_levels)),dt_through_lanes,by="group",all.x=T)
  
  # Summarize surface type
  dt_surftype <- unique(dt[dataitem == 'SURFACE_TYPE',
                        .(dataitem, routeid, section_id, beginpoint_og, endpoint_og, valuenumeric, group)])
  dt_surf_sum <- dt_surftype[,
            list(n_missing = sum((is.na(valuenumeric) | is.null(valuenumeric))),
            n_1 =  sum((valuenumeric == 1 & !is.na(valuenumeric))),
            n_11 = sum((valuenumeric == 11 & !is.na(valuenumeric))),
            n_sec_gt_011 = sum(((endpoint_og - beginpoint_og) >= 0.11))),
       by=group]
  
  dt_surf_sum = merge(data.table(group=1:length(gF_SYSTEM_levels)),
                      dt_surf_sum, by="group", all.x=T)
  
  tab = merge(dt_through_lanes, dt_surf_sum, by="group")
  tab[, group := gF_SYSTEM_levels[as.numeric(tab[, group])]]
    
  thm <- ttheme_default(
    core    = list(fg_params=list(col=gColors$text, fontsize=5.5, hjust=1, x=0.95),
                   bg_params=list(fill='grey95'),
                   padding=unit(c(0.1, 0.1), 'inches')),
    colhead = list(fg_params=list(col=gColors$text, fontsize=6.0,
                                  fontface='bold', hjust=1, x=0.95),
                   bg_params=list(fill=gColors$text_background),
                   padding=unit(c(0.1, 0.1), 'inches')))
  
  setnames(tab,"group","Functional\nSystem")
  setnames(tab,"n_missing.x","Missing\nThrough Lanes")
  setnames(tab,"n_missing.y","Missing\nSurface Type")
  setnames(tab,"n_1","Surface Type\nEquals 1")
  setnames(tab,"n_11","Surface Type\nEquals 11")
  setnames(tab,"n_sec_gt_011","Sections\n>= 0.11 miles")
  
  obj = tableGrob(tab,rows=NULL,theme = thm)
  
  return(obj)
  
  #tab <- table(dt_surftype$valuenumeric, useNA='always')
  
  #stop('need to create a table or graphic for inclusion in grid.arrange')
}
