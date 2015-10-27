###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2015
# Author: Jeff Dumont
#
#
# Description:
#
# Author needs to add a description!
#
###########################################################################

create_overall_condition <- function(data,state,year,population)
{
     # the algorithm is described in detail in the file title MAP-21 Pavement Condition Rule
  
     # four metrics are used based on the surface type
     rutting  <- data[state_code==state&year_record==year&data_item=="RUTTING",list(route_id,begin_point,end_point,value_numeric),]
     setnames(rutting,"value_numeric","rutting")
     iri      <- data[state_code==state&year_record==year&data_item=="IRI",list(route_id,begin_point,end_point,value_numeric),]
     setnames(iri,"value_numeric","iri")
     faulting <- data[state_code==state&year_record==year&data_item=="FAULTING",list(route_id,begin_point,end_point,value_numeric),]
     setnames(faulting,"value_numeric","faulting")
     cracking <- data[state_code==state&year_record==year&data_item=="CRACKING_PERCENT",list(route_id,begin_point,end_point,value_numeric),]
     setnames(cracking,"value_numeric","cracking")
     
     surface <- data[state_code==state&year_record==year&data_item=="SURFACE_TYPE",list(route_id,F_SYSTEM,Interstate,NHS,begin_point,end_point,value_numeric),]
     setnames(surface,"value_numeric","surface")
     
     urban <- data[state_code==state&year_record==year&data_item=="URBAN_CODE",list(route_id,begin_point,end_point,value_numeric),]
     setnames(urban,"value_numeric","urban_code")
     
     urban[urban_code==99999,rural:=1]
     urban[!is.na(urban_code)&is.na(rural),rural:=0]
     
     iri <- sqldf("select A.*,B.urban_code,B.rural from iri A left join urban B on A.route_id = B.route_id and A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point")
     iri <- data.table(iri)
     
     iri <- merge(iri,population,by="urban_code",all.x=TRUE,all.y=FALSE)
     
     iri[rural==1,threshold:=170]
     iri[(rural==0)*(pop<1000000),threshold:=170]
     iri[(rural==0)*(pop>=1000000),threshold:=220]
     
     rutting[(rutting<0.20)               ,rscore:= "G"] # good 
     rutting[(rutting>=0.20&rutting<=0.40),rscore:= "F"] # fair
     rutting[(rutting>0.40)               ,rscore:= "P"] # poor
     
     faulting[(faulting< 0.05)               ,fscore:= "G"] # good
     faulting[(faulting>=0.05&faulting<=0.15),fscore:= "F"] # fair
     faulting[(faulting> 0.15)               ,fscore:= "P"] # poor
     
     cracking[(cracking<5)              ,cscore:= "G"] # good
     cracking[(cracking>=5&cracking<=10),cscore:= "F"] # fair
     cracking[(cracking>10)             ,cscore:= "P"] # poor

     # iri is a function of population size and urban code
     
     iri[(iri< 95)                         ,iscore:= "G"] # good 
     iri[(iri>=95)       & (iri<=threshold),iscore:= "F"] # fair
     iri[(iri> threshold)                  ,iscore:= "P"] # poor
     
     condition <- merge(surface,  rutting, by=c("route_id","begin_point","end_point"),all.x=TRUE,all.y=FALSE)
     condition <- merge(condition,faulting,by=c("route_id","begin_point","end_point"),all.x=TRUE,all.y=FALSE)
     condition <- merge(condition,cracking,by=c("route_id","begin_point","end_point"),all.x=TRUE,all.y=FALSE)
     condition <- merge(condition,iri,     by=c("route_id","begin_point","end_point"),all.x=TRUE,all.y=FALSE)
     
     #table 4.5 in the manual is very useful
     
     # rutting: 2,6,7,8
     # faulting: 3,4,9,10
     # CRCP: 5
     
     #condition[,oscore:=NA]
     #if(nrow(rutting)>0&nrow(iri)>0&nrow(cracking)>0)
     #{
          condition[surface %in% c(2,6,7,8) &!is.na(rscore)&!is.na(cscore)&!is.na(iscore),oscore:=paste0(rscore,cscore,iscore)]
     #}
     #if(nrow(faulting)>0&nrow(iri)>0&nrow(cracking)>0)
     #{
          condition[surface %in% c(3,4,9,10)&!is.na(fscore)&!is.na(cscore)&!is.na(iscore),oscore:=paste0(fscore,cscore,iscore)]
     #}
     #if(nrow(iri)>0&nrow(cracking)>0)
     #{
          condition[surface %in% c(5)       &               !is.na(cscore)&!is.na(iscore),oscore:=paste0(       cscore,iscore)]
     #}
     
     countChar <- function(x,stringtofind)
     {
          return(length(regmatches(x,gregexpr(stringtofind,x))[[1]]))
     }
     
     condition[nchar(oscore)==3,overallscore:=1*(countChar(oscore,"G")==3)+3*(countChar(oscore,"P")>=2),by=.(route_id,begin_point,end_point)]
     condition[!is.na(oscore)&overallscore==0&nchar(oscore)==3,overallscore:=2]
     condition[nchar(oscore)==2,overallscore:=1*(countChar(oscore,"G")==2)+3*(countChar(oscore,"P")==2),by=.(route_id,begin_point,end_point)]
     condition[!is.na(oscore)&overallscore==0&nchar(oscore)==2,overallscore:=2]
     
     condition[,overallscore:=c(NA,"G","F","P")[1+overallscore]]    
     
     results.interstate <- table(condition[Interstate==1,overallscore])
     results.nhs        <- table(condition[NHS==1,overallscore])
     results.fsystem1   <- table(condition[F_SYSTEM==1,overallscore])
     results.fsystem2   <- table(condition[F_SYSTEM==2,overallscore])
     
     if(nrow(results.interstate)>0)
     {
          p1 <- create_donut_chart(results.interstate)
     } else
     {
          p1 <- textGrob(NoDataString,gp=gpar(fontsize=12, col="Red"))
     }
     
     if(nrow(results.nhs)>0)
     {
          p2 <- create_donut_chart(results.nhs)
     } else
     {
          p2 <- textGrob(NoDataString,gp=gpar(fontsize=12, col="Red"))
     }
     
     if(nrow(results.fsystem1)>0)
     {
          p3 <- create_donut_chart(results.fsystem1)
     } else
     {
          p3 <- textGrob(NoDataString,gp=gpar(fontsize=12, col="Red"))
     }
     
     if(nrow(results.fsystem2)>0)
     {
          p4 <- create_donut_chart(results.fsystem2)
     } else
     {
          p4 <- textGrob(NoDataString,gp=gpar(fontsize=12, col="Red"))
     }
     
     obj <- arrangeGrob(
                         # row 1
                         rectGrob(gp=gpar(fill="white",col="white")), 
                         rectGrob(gp=gpar(fill="white",col="white")), 
                         rectGrob(gp=gpar(fill="white",col="white")), 
                         rectGrob(gp=gpar(fill="white",col="white")), 
                         rectGrob(gp=gpar(fill="white",col="white")), 
                         
                         # row 2
                         rectGrob(gp=gpar(fill="white",col="white")), 
                         textGrob("Interstate",gp=gpar(col="slategray",fontface="bold",fontsize=9)),
                         textGrob("National Highway System",gp=gpar(col="slategray",fontface="bold",fontsize=9)),
                         textGrob("Principal Arterial Systems",gp=gpar(col="slategray",fontface="bold",fontsize=9)),
                         textGrob("Lower Level Systems",gp=gpar(col="slategray",fontface="bold",fontsize=9)),
          
                         # row 3
                         # labels
                         arrangeGrob(rectGrob(gp=gpar(fill="white",col="white"))          ,rectGrob(gp=gpar(fill="white",col="white")),
                                     textGrob("Good",hjust=1,gp=gpar(col="slategray")),            rectGrob(,gp=gpar(fill="gray65",col="gray65")),
                                     rectGrob(gp=gpar(fill="white",col="white"))          ,rectGrob(gp=gpar(fill="white",col="white")),
                                     textGrob("Fair",hjust=1,gp=gpar(col="slategray")),            rectGrob(gp=gpar(fill="gray85",col="gray85"))        ,
                                     rectGrob(gp=gpar(fill="white",col="white"))          ,rectGrob(gp=gpar(fill="white",col="white")),
                                     textGrob("Poor",hjust=1,gp=gpar(col="slategray")),            rectGrob(gp=gpar(fill="red",col="red"))          ,
                                     rectGrob(gp=gpar(fill="white",col="white"))          ,rectGrob(gp=gpar(fill="white",col="white")),
                                     ncol=2,nrow=7,widths=unit(c(0.8,0.2),units="npc"),heights=c(0.1,0.7/3,0.05,0.7/3,0.05,0.7/3,0.1)
                         ),
                         # plots
                        p1,p2,p3,p4,
                        ncol=5,nrow=3,
                        heights=c(0.05,0.1,0.85),
                        widths=unit(c(0.15,rep(0.85/4,4)),units="npc"))
     
     return(obj)
     
}