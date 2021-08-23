###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont
#
#
# Description:
#
# Creates overall report by functional system. No longer used.
# This function produces as graphical object as an overall report for the
# first page by functional system.
#
###########################################################################

create_overall_report_byFSYSTEM <- function(
     data,
     state,
     year,
     year_compare
)
{
     numberOfRecords   <- data[state_code==state&year_record==year,.N,by=list(F_SYSTEM)]     
     numberOfVariables <- data[state_code==state&year_record==year,length(unique(data_item)),by=list(F_SYSTEM)]
     numberRoutes      <- unique(data[state_code==state&year_record==year,list(route_id,F_SYSTEM)])[,.N,by=list(F_SYSTEM)]
     numberSegments    <- unique(data[state_code==state&year_record==year,list(route_id,begin_point,end_point,F_SYSTEM),])[,.N,by=list(F_SYSTEM)]
     
     result <- merge(numberOfRecords,numberOfVariables,by=c("F_SYSTEM"))
     result[,N:=string_format(N)]
     result[,V1:=string_format(V1)]
     setnames(result,"N","Number of\nRecords")
     setnames(result,"V1","Number of\nVariables")
     result <- merge(result,numberRoutes,by=c("F_SYSTEM"))
     result[,N:=string_format(N)]
     setnames(result,"N","Number of\nRoutes")
     result <- merge(result,numberSegments,by=c("F_SYSTEM"))
     result[,N:=string_format(N)]
     setnames(result,"N","Number of\nSegments")
     
     result <- merge(data.table(F_SYSTEM=1:7),result,by="F_SYSTEM",all.x=T)
     result[,F_SYSTEM:=gF_SYSTEM_levels[as.numeric(F_SYSTEM)],]
     
     setnames(result,"F_SYSTEM","Functional\nSystem")
     
     thm <- ttheme_default(
       core    = list(fg_params=list(col=gColors$text, fontsize=6.5, hjust=1, x=0.95),
                      bg_params=list(fill='grey95'),
                      padding=unit(c(0.1, 0.1), 'inches')),
       colhead = list(fg_params=list(col=gColors$text, fontsize=7.0,
                                     fontface='bold', hjust=1, x=0.95),
                      bg_params=list(fill='grey90'),
                      padding=unit(c(0.1, 0.1), 'inches')))
     
     ob <- tableGrob(result, rows=NULL, theme=thm)
     
     ob <- vertically_align(ob)
     
     return(ob)
     
}
