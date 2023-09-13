###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont
#
#
# Description:
#
# This function applies the MAP-21 pavement condition rule and returns a
# table of results that can be graphically presented.
#
###########################################################################

create_overall_condition <- function(data, state, year, population)
{
 
     # the algorithm is described in detail in the file title MAP-21 Pavement Condition Rule
  
     # four metrics are used based on the surface type
     rutting  <- data[stateid  ==  state & datayear == year & dataitem == "RUTTING" & FACILITY_TYPE!=4,
                      list(routeid, beginpoint, endpoint, value_numeric), ]
     setnames(rutting, "value_numeric", "rutting")

     iri      <- data[stateid == state & datayear == year & dataitem == "IRI" & FACILITY_TYPE!=4,
                      list(routeid, beginpoint, endpoint, value_numeric), ]
     setnames(iri, "value_numeric", "iri")
     
     faulting <- data[stateid == state & datayear == year & dataitem == "FAULTING" & FACILITY_TYPE!=4,
                      list(routeid, beginpoint, endpoint, value_numeric), ]
     setnames(faulting, "value_numeric", "faulting")
     
     cracking <- data[stateid == state & datayear == year & dataitem == "CRACKING_PERCENT" & FACILITY_TYPE!=4,
                      list(routeid, beginpoint, endpoint, value_numeric), ]
     setnames(cracking, "value_numeric", "cracking")
     
     surface <- data[stateid == state & datayear == year & dataitem == "SURFACE_TYPE" & FACILITY_TYPE!=4,
                     list(routeid, F_SYSTEM, Interstate, NHS, beginpoint, endpoint, value_numeric), ]
     setnames(surface, "value_numeric", "surface")
     
     urban <- data[stateid == state & datayear == year & dataitem == "URBAN_ID" & FACILITY_TYPE!=4,
                   list(routeid, beginpoint, endpoint, value_numeric), ]
     setnames(urban, "value_numeric", "urban_id")
     
     if(nrow(rutting) == 0 | 
        nrow(iri) == 0 | 
        nrow(faulting) == 0 | 
        nrow(cracking) == 0 |
        nrow(surface) == 0)
     {
       # exit if not enough data to complete the analysis 
       return(textGrob(NoDataString, gp=gpar(fontsize=8,  col=gColors$highlight)))
     } else {
       urban[, rural:=0]
       urban[urban_id == 99999, rural:=1]
       urban[!is.na(urban_id) & is.na(rural), rural:=0]
       
       iri <- sqldf("select A.*, B.urban_id, B.rural from iri A left join urban B on A.routeid = B.routeid and A.beginpoint between B.beginpoint and B.endpoint and A.endpoint between B.beginpoint and B.endpoint")
       iri <- data.table(iri)
       
       iri <- merge(iri, population, by="urban_id", all.x=TRUE, all.y=FALSE)
       
       iri[rural == 1, threshold:=170]
       iri[(rural == 0)*(pop<1000000), threshold:=170]
       iri[(rural == 0)*(pop>=1000000), threshold:=220]
       
       rutting[(rutting<0.20)                 , rscore:= "G"] # good 
       rutting[(rutting>=0.20 & rutting<=0.40), rscore:= "F"] # fair
       rutting[(rutting>0.40)                 , rscore:= "P"] # poor
       
       faulting[(faulting< 0.05)                 , fscore:= "G"] # good
       faulting[(faulting>=0.05 & faulting<=0.15), fscore:= "F"] # fair
       faulting[(faulting> 0.15)                 , fscore:= "P"] # poor
       
       cracking[(cracking<5)                , cscore:= "G"] # good
       cracking[(cracking>=5 & cracking<=10), cscore:= "F"] # fair
       cracking[(cracking>10)               , cscore:= "P"] # poor
  
       # iri is a function of population size and urban code
       
       iri[(iri< 95)                         , iscore:= "G"] # good 
       iri[(iri>=95) & (iri<=threshold)      , iscore:= "F"] # fair
       iri[(iri> threshold)                  , iscore:= "P"] # poor
       
       # Join cracking
       condition <- sqldf("select A.*,  B.cracking,  B.cscore
                          from iri A 
                          left join cracking B on 
                            A.routeid = B.routeid and (
                            ( A.beginpoint between B.beginpoint and B.endpoint and A.endpoint between B.beginpoint and B.endpoint ) or
                            ( B.beginpoint between A.beginpoint and A.endpoint and B.endpoint between A.beginpoint and A.endpoint )
                          )")
       
       # Join faulting
       condition <- sqldf("select A.*,  B.faulting,  B.fscore
                          from condition A 
                         left join faulting B on 
                          A.routeid = B.routeid and (
                         ( A.beginpoint between B.beginpoint and B.endpoint and A.endpoint between B.beginpoint and B.endpoint ) or
                         ( B.beginpoint between A.beginpoint and A.endpoint and B.endpoint between A.beginpoint and A.endpoint )
                     )")
       
       # Join rutting
       condition <- sqldf("select A.*,  B.rutting,  B.rscore
                          from condition A 
                          left join rutting B on 
                            A.routeid = B.routeid and (
                            ( A.beginpoint between B.beginpoint and B.endpoint and A.endpoint between B.beginpoint and B.endpoint ) or
                            ( B.beginpoint between A.beginpoint and A.endpoint and B.endpoint between A.beginpoint and A.endpoint )
                          )")
       
       # Join surface
       condition <- sqldf("select A.*,  B.surface, B.Interstate, B.NHS
                          from condition A 
                          left join surface B on 
                            A.routeid = B.routeid and (
                            ( A.beginpoint between B.beginpoint and B.endpoint and A.endpoint between B.beginpoint and B.endpoint ) or
                            ( B.beginpoint between A.beginpoint and A.endpoint and B.endpoint between A.beginpoint and A.endpoint )
                          )")
       
      condition <- data.table(condition)
       
       #table 4.5 in the manual is very useful
       
       # rutting: 2, 6, 7, 8
       # faulting: 3, 4, 9, 10
       # CRCP: 5
       
       
       # asphalt 
       condition[surface %in% c(2, 6, 7, 8)  & !is.na(rscore) & !is.na(cscore) & !is.na(iscore), oscore:=paste0(rscore, cscore, iscore)]

       # jointed concrete
       # added surface type 11 (other) as concrete to match Max Grogg's process
       condition[surface %in% c(3, 4, 9, 10, 11) & !is.na(fscore) & !is.na(cscore) & !is.na(iscore), oscore:=paste0(fscore, cscore, iscore)]

       # Continuously reinforced concrete
       condition[surface %in% c(5)        &                !is.na(cscore) & !is.na(iscore), oscore:=paste0(       cscore, iscore)]
       
       countChar <- function(x, stringtofind)
       {
            return(length(regmatches(x, gregexpr(stringtofind, x))[[1]]))
       }
       
       condition[nchar(oscore) == 3, overallscore:=1*(countChar(oscore, "G") == 3)+3*(countChar(oscore, "P")>=2), by=.(routeid, beginpoint, endpoint)]
       condition[!is.na(oscore) & overallscore == 0 & nchar(oscore) == 3, overallscore:=2]
       condition[nchar(oscore) == 2, overallscore:=1*(countChar(oscore, "G") == 2)+3*(countChar(oscore, "P") == 2), by=.(routeid, beginpoint, endpoint)]
       condition[!is.na(oscore) & overallscore == 0 & nchar(oscore) == 2, overallscore:=2]
       
       condition[, overallscore:=c(NA, "G", "F", "P")[1+overallscore]]    
       
       results.interstate <- condition[Interstate == 1, list(.N, miles=sum(endpoint-beginpoint)),  by=.(overallscore)]
       results.NHS        <- condition[NHS == 1, list(.N, miles=sum(endpoint-beginpoint)),  by=.(overallscore)]
       
       if(nrow(results.interstate)>0)
       {
            p1 <- barOCPlot(results.interstate, "Interstate")
       } else
       {
            p1 <- textGrob(NoDataString, gp=gpar(fontsize=8,  col=gColors$highlight))
       }
       
       if(nrow(results.NHS) > 0)
       {
            p2 <- barOCPlot(results.NHS, "NHS")
       } else
       {
            p2 <- textGrob(NoDataString, gp=gpar(fontsize=8,  col=gColors$highlight))
       }
       
       groblist <- list(textGrob(''),
                        textGrob("The algorithm applies the MAP-21\ncondition rule to classify\nsections as good,  fair or poor.", 
                                 hjust=1, gp=gpar(col=gColors$dark, fontface="bold", fontsize=7)), 
                        p1,
                        textGrob(""),
                        p2) 
                        
       obj <- arrangeGrob(grobs = groblist,
         ncol=5, nrow=1, widths=unit(c(0.025, 0.3, 0.65/2, 0.025, 0.65/2), units="npc"), hjust=0.5)
  
       return(obj)
     }
     
}